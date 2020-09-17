import React, { PureComponent } from 'react';
import _ from 'lodash';

interface VirtualScrollerProps {
  origin: 'top' | 'bottom';
  loadRows(start: number, end: number): void;
  data: Map<number, any>;
  renderer(index): JSX.Element | null;
  onStartReached?(): void;
  onEndReached?(): void;
  size: number;
  onCalculateVisibleItems?(visibleItems: Map<number, JSX.Element>): void;
  onScroll?({ scrollTop, scrollHeight, windowHeight }): void;
  style?: any;
}

interface VirtualScrollerState {
  startgap: number | undefined;
  visibleItems: Map<number, Element>;
  endgap: number | undefined;
  totalHeight: number;
  averageHeight: number;
  scrollTop: number;
}

export default class VirtualScroller extends PureComponent<VirtualScrollerProps, VirtualScrollerState> {
  private scrollContainer: React.RefObject<HTMLDivElement>;
  public window: HTMLDivElement | null;
  private cache: Map<number, any>;
  private pendingLoad: {
    start: number;
    end: number
    timeout: ReturnType<typeof setTimeout>;
  } | undefined;

  OVERSCAN_SIZE = 100; // Minimum number of messages on either side before loadRows is called

  constructor(props) {
    super(props);
    this.state = {
      startgap: props.origin === 'top' ? 0 : undefined,
      visibleItems: new Map(),
      endgap: props.origin === 'bottom' ? 0 : undefined,
      totalHeight: 0,
      averageHeight: 64,
      scrollTop: props.origin === 'top' ? 0 : Infinity
    };

    this.scrollContainer = React.createRef();
    this.window = null;
    this.cache = new Map();

    this.recalculateTotalHeight = this.recalculateTotalHeight.bind(this);
    this.calculateVisibleItems = this.calculateVisibleItems.bind(this);
    this.estimateIndexFromScrollTop = this.estimateIndexFromScrollTop.bind(this);
    this.invertedKeyHandler = this.invertedKeyHandler.bind(this);
    this.heightOf = this.heightOf.bind(this);
    this.setScrollTop = this.setScrollTop.bind(this);
    this.scrollToData = this.scrollToData.bind(this);
    this.loadRows = _.memoize(this.loadRows).bind(this);
  }

  componentDidMount() {
    this.calculateVisibleItems();
  }

  componentDidUpdate(prevProps: VirtualScrollerProps, prevState: VirtualScrollerState) {
    const {
      scrollContainer, window,
      props: { origin },
      state: { totalHeight, scrollTop }
    } = this;
  }

  scrollToData(targetIndex: number): Promise<void> {
    if (!this.window) {
      return new Promise((resolve, reject) => {reject()});
    }
    const { offsetHeight } = this.window;
    let scrollTop = 0;
    let itemHeight = 0;
    new Map([...this.props.data].reverse()).forEach((datum, index) => {
      const height = this.heightOf(index);
      if (index >= targetIndex) {
        scrollTop += height;
        if (index === targetIndex) {
          itemHeight = height;
        }
      }
    });
    return this.setScrollTop(scrollTop - (offsetHeight / 2) + itemHeight);
  }

  recalculateTotalHeight() {
    let { averageHeight } = this.state;
    let totalHeight = 0;
    this.props.data.forEach((datum, index) => {
      totalHeight += this.heightOf(index);
    });
    averageHeight = Number((totalHeight / this.props.data.size).toFixed());
    totalHeight += (this.props.size - this.props.data.size) * averageHeight;
    this.setState({ totalHeight, averageHeight });
  }

  estimateIndexFromScrollTop(targetScrollTop: number): number | void {
    if (!this.window) return;
    let index = this.props.size;
    const { averageHeight } = this.state;
    let height = 0;
    while (height < targetScrollTop) {
      const itemHeight = this.cache.has(index) ? this.cache.get(index).height : averageHeight;
      height += itemHeight;
      index--;
    }
    return index;
  }

  heightOf(index: number): number {
    return this.cache.has(index) ? this.cache.get(index).height : this.state.averageHeight;
  }

  calculateVisibleItems() {
    if (!this.window) return;
    let startgap = 0, heightShown = 0, endgap = 0;
    let startGapFilled = false;
    let visibleItems = new Map();
    let startBuffer = new Map();
    let endBuffer = new Map();
    const { scrollTop, offsetHeight: windowHeight } = this.window;
    const { averageHeight } = this.state;
    const { data, size: totalSize, onCalculateVisibleItems } = this.props;

    const items = new Map([...data].reverse());

    items.forEach((datum, index) => {
      const height = this.heightOf(index);
      if (startgap < scrollTop && !startGapFilled) {
        startBuffer.set(index, datum);
        startgap += height;
      } else if (heightShown < windowHeight) {
        startGapFilled = true;
        visibleItems.set(index, datum);
        heightShown += height;
      } else if (endBuffer.size < visibleItems.size) {
        endBuffer.set(index, data.get(index));
      } else {
        endgap += height;
      }
    });

    endgap += Math.abs(totalSize - data.size) * averageHeight;
    startBuffer = new Map([...startBuffer].reverse().slice(0, visibleItems.size));
    
    startBuffer.forEach((datum, index) => {
      startgap -= this.heightOf(index);
    });
    
    visibleItems = new Map([...visibleItems].reverse());
    endBuffer = new Map([...endBuffer].reverse());
    const firstVisibleKey = Array.from(visibleItems.keys())[0] ?? this.estimateIndexFromScrollTop(scrollTop);
    const firstNeededKey = Math.max(firstVisibleKey - this.OVERSCAN_SIZE, 0);
    if (!data.has(firstNeededKey + 1)) {
      this.loadRows(firstNeededKey, firstVisibleKey - 1);
    }
    const lastVisibleKey = Array.from(visibleItems.keys())[visibleItems.size - 1] ?? this.estimateIndexFromScrollTop(scrollTop + windowHeight);
    const lastNeededKey = Math.min(lastVisibleKey + this.OVERSCAN_SIZE, totalSize)
    if (!data.has(lastNeededKey - 1)) {
      this.loadRows(lastVisibleKey + 1, lastNeededKey);
    }
    onCalculateVisibleItems ? onCalculateVisibleItems(visibleItems) : null;
    this.setState({
      startgap: Number(startgap.toFixed()),
      visibleItems: new Map([...endBuffer, ...visibleItems, ...startBuffer]),
      endgap: Number(endgap.toFixed()),
    });
  }

  loadRows(start, end) {
    if (isNaN(start) || isNaN(end)) return;
    if (this.pendingLoad?.timeout) {
      clearTimeout(this.pendingLoad.timeout);
      start = Math.min(start, this.pendingLoad.start);
      end = Math.max(end, this.pendingLoad.end);
    }
    this.pendingLoad = {
      timeout: setTimeout(() => {
        if (!this.pendingLoad) return;
        start = Math.max(this.pendingLoad.start, 0);
        end = Math.min(Math.max(this.pendingLoad.end, 0), this.props.size);
        if (start < end) {
          this.props.loadRows(start, end);
        }
        clearTimeout(this.pendingLoad.timeout);
        this.pendingLoad = undefined;
      }, 500),
      start, end
    };
  }

  invertedKeyHandler(event): void | false {
    if (event.code === 'ArrowUp' || event.code === 'ArrowDown') {
      event.preventDefault();
      event.stopImmediatePropagation();
      if (event.code === 'ArrowUp') {
        this.window.scrollBy(0, 30);
      } else if (event.code === 'ArrowDown') {
        this.window.scrollBy(0, -30);
      }
      return false;
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.invertedKeyHandler, true);
  }
  
  setWindow(element) {
    if (this.window) return;
    this.window = element;
    if (this.props.origin === 'bottom') {
      element.addEventListener('wheel', (event) => {
        event.preventDefault();
        element.scrollBy(0, event.deltaY * -1);
        return false;
      }, { passive: false });
      window.addEventListener('keydown', this.invertedKeyHandler, { passive: false });
    }
    this.resetScroll();
  }

  resetScroll(): Promise<void> {
    if (!this.window) return new Promise((resolve, reject) => {reject()});
    return this.setScrollTop(0);
  }

  setScrollTop(distance: number, delay: number = 100): Promise<void> {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        if (!this.window) {
          reject();
          return;
        }
        this.window.scrollTop = distance;
        resolve();
      }, delay);
    });
  }

  onScroll(event) {
    if (!this.window) return;
    const { onStartReached, onEndReached, onScroll } = this.props;
    const windowHeight = this.window.offsetHeight
    const { scrollTop, scrollHeight } = this.window;
    if (scrollTop !== scrollHeight) {
      this.setState({ scrollTop });
    }
    
    this.calculateVisibleItems();
    onScroll ? onScroll({ scrollTop, scrollHeight, windowHeight }) : null;
    if (scrollTop === 0) {
      if (onStartReached) onStartReached();
    } else if (scrollTop + windowHeight >= scrollHeight) {
      if (onEndReached) onEndReached();
    }
  }

  render() {
    const {
      startgap,
      endgap,
      visibleItems
    } = this.state;
    
    const {
      origin = 'top',
      loadRows,
      renderer,
      style,
      data
    } = this.props;
    
    const indexesToRender = Array.from(visibleItems.keys());

    const transform = origin === 'top' ? 'scaleY(1)' : 'scaleY(-1)';

    const render = (index) => {
      const measure = (element) => {
        if (element) {
          this.cache.set(index, {
            height: element.offsetHeight,
            element
          });
          _.debounce(this.recalculateTotalHeight, 500)();
        }
      };
      return renderer({ index, measure, scrollWindow: this.window });
    };

    return (
      <div ref={this.setWindow.bind(this)} onScroll={this.onScroll.bind(this)} style={{...style, ...{overflowY: 'scroll', transform }}}>
        <div ref={this.scrollContainer} style={{ transform }}>
          <div style={{ height: `${origin === 'top' ? startgap : endgap}px` }}></div>
          {indexesToRender.map(render)}
          <div style={{ height: `${origin === 'top' ? endgap : startgap}px` }}></div>
        </div>
      </div>
    );
  }
}