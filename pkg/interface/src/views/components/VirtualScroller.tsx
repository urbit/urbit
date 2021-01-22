import React, { PureComponent, Component } from 'react';
import _ from 'lodash';
import { BigIntOrderedMap } from "~/logic/lib/BigIntOrderedMap";
import normalizeWheel from 'normalize-wheel';
import { Box } from '@tlon/indigo-react';
import bigInt, { BigInteger } from 'big-integer';
import * as bigIntUtils from '~/logic/lib/bigInt';

interface RendererProps {
  index: BigInteger;
  measure: (el: any) => void;
  scrollWindow: any
}

interface VirtualScrollerProps {
  origin: 'top' | 'bottom';
  loadRows(newer: boolean): void;
  data: BigIntOrderedMap<BigInteger, any>;
  renderer: (props: RendererProps) => JSX.Element | null;
  onStartReached?(): void;
  onEndReached?(): void;
  size: number;
  onCalculateVisibleItems?(visibleItems: BigIntOrderedMap<BigInteger, JSX.Element>): void;
  onScroll?({ scrollTop, scrollHeight, windowHeight }): void;
  style?: any;
}

interface VirtualScrollerState {
  startgap: number | undefined;
  visibleItems: BigIntOrderedMap<BigInteger, Element>;
  endgap: number | undefined;
  totalHeight: number;
  averageHeight: number;
  scrollTop: number;
}

export default class VirtualScroller extends Component<VirtualScrollerProps, VirtualScrollerState> {
  private scrollContainer: React.RefObject<HTMLDivElement>;
  public window: HTMLDivElement | null;
  private cache: BigIntOrderedMap<any>;
  private pendingLoad: {
    start: BigInteger;
    end: BigInteger
    timeout: ReturnType<typeof setTimeout>;
  } | undefined;

  overscan = 150;

  OVERSCAN_SIZE = 100; // Minimum number of messages on either side before loadRows is called

  constructor(props: VirtualScrollerProps) {
    super(props);
    this.state = {
      startgap: props.origin === 'top' ? 0 : undefined,
      visibleItems: new BigIntOrderedMap(),
      endgap: props.origin === 'bottom' ? 0 : undefined,
      totalHeight: 0,
      averageHeight: 130,
      scrollTop: props.origin === 'top' ? 0 : undefined
    };

    this.scrollContainer = React.createRef();
    this.window = null;
    this.cache = new BigIntOrderedMap();

    this.recalculateTotalHeight = _.throttle(this.recalculateTotalHeight.bind(this), 200);
    this.calculateVisibleItems = _.throttle(this.calculateVisibleItems.bind(this), 200);
    this.estimateIndexFromScrollTop = this.estimateIndexFromScrollTop.bind(this);
    this.invertedKeyHandler = this.invertedKeyHandler.bind(this);
    this.heightOf = this.heightOf.bind(this);
    this.setScrollTop = this.setScrollTop.bind(this);
    this.scrollToData = this.scrollToData.bind(this);
    this.scrollKeyMap = this.scrollKeyMap.bind(this);
    this.loadRows = _.debounce(this.loadRows, 300, { leading: true }).bind(this);
  }

  componentDidMount() {
    this.calculateVisibleItems();

    this.recalculateTotalHeight();
  }

  componentDidUpdate(prevProps: VirtualScrollerProps, prevState: VirtualScrollerState) {
    const {
      scrollContainer, window,
      props: { origin },
      state: { totalHeight, scrollTop }
    } = this;
  }

  scrollToData(targetIndex: BigInteger): Promise<void> {
    if (!this.window) {
      return new Promise((resolve, reject) => {reject()});
    }
    const { offsetHeight } = this.window;
    let scrollTop = 0;
    let itemHeight = 0;
    new BigIntOrderedMap([...this.props.data].reverse()).forEach((datum, index) => {
      const height = this.heightOf(index);
      if (index.geq(targetIndex)) {
        scrollTop += height;
        if (index.eq(targetIndex)) {
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
      totalHeight += Math.max(this.heightOf(index), 0);
    });
    averageHeight = Number((totalHeight / this.props.data.size).toFixed());
    totalHeight += (this.props.size - this.props.data.size) * averageHeight;
    this.setState({ totalHeight, averageHeight });
  }

  estimateIndexFromScrollTop(targetScrollTop: number): BigInteger | undefined {
    if (!this.window) return undefined;
    let index = bigInt(this.props.size);
    const { averageHeight } = this.state;
    let height = 0;
    while (height < targetScrollTop) {
      const itemHeight = this.cache.has(index) ? this.cache.get(index).height : averageHeight;
      height += itemHeight;
      index.subtract(bigInt.one);
    }
    return index;
  }

  heightOf(index: BigInteger): number {
    return this.cache.has(index) ? this.cache.get(index).height : this.state.averageHeight;
  }

  calculateVisibleItems() {
    if (!this.window) return;
    let startgap = 0, heightShown = 0, endgap = 0;
    let startGapFilled = false;
    let visibleItems = new BigIntOrderedMap<any>();
    const { scrollTop, offsetHeight: windowHeight } = this.window;
    const { averageHeight, totalHeight } = this.state;
    const { data, size: totalSize, onCalculateVisibleItems } = this.props;


    [...data].forEach(([index, datum]) => {
      const height = this.heightOf(index);
      if (startgap < (scrollTop - this.overscan) && !startGapFilled) {
        startgap += height;
      } else if (heightShown < (windowHeight + this.overscan)) {
        startGapFilled = true;
        visibleItems.set(index, datum);
        heightShown += height;
      } 
    });

    endgap = totalHeight - heightShown - startgap;

    const firstVisibleKey = visibleItems.peekSmallest()?.[0] ?? this.estimateIndexFromScrollTop(scrollTop)!;
    const smallest = data.peekSmallest();
    if (smallest && smallest[0].eq(firstVisibleKey)) {
      this.loadRows(false);
    }
    const lastVisibleKey =
      visibleItems.peekLargest()?.[0]
      ?? bigInt(this.estimateIndexFromScrollTop(scrollTop + windowHeight)!);

    const largest = data.peekLargest();

    if (largest && largest[0].eq(lastVisibleKey)) {
      this.loadRows(true);
    }
    onCalculateVisibleItems ? onCalculateVisibleItems(visibleItems) : null;
    this.setState({
      startgap: Number(startgap.toFixed()),
      visibleItems,
      endgap: Number(endgap.toFixed()),
    });
  }

  loadRows(newer: boolean) {
    this.props.loadRows(newer);
  }

  scrollKeyMap(): Map<string, number> {
    return new Map([
      ['ArrowUp', this.state.averageHeight],
      ['ArrowDown', this.state.averageHeight * -1],
      ['PageUp', this.window!.offsetHeight],
      ['PageDown', this.window!.offsetHeight * -1],
      ['Home', this.window!.scrollHeight],
      ['End', this.window!.scrollHeight * -1],
      ['Space', this.window!.offsetHeight * -1]
    ]);
  }

  invertedKeyHandler(event): void | false {
    const map = this.scrollKeyMap();
    if (map.has(event.code) && document.body.isSameNode(document.activeElement)) {
      event.preventDefault();
      event.stopImmediatePropagation();
      let distance = map.get(event.code)!;
      if (event.code === 'Space' && event.shiftKey) {
        distance = distance * -1;
      }
      this.window!.scrollBy(0, distance);
      return false;
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.invertedKeyHandler);
  }

  setWindow(element) {
    if (!element) return;
    if (this.window) {
      if (this.window.isSameNode(element)) {
        return;
      } else {
        window.removeEventListener('keydown', this.invertedKeyHandler);
      }
    }

    this.overscan = Math.max(element.offsetHeight * 3, 500);

    this.window = element;
    if (this.props.origin === 'bottom') {
       element.addEventListener('wheel', (event) => {
        event.preventDefault();
        const normalized = normalizeWheel(event);
        element.scrollBy(0, normalized.pixelY * -1);
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

    const indexesToRender = origin === 'top' ? visibleItems.keys() : visibleItems.keys().reverse();

    const transform = origin === 'top' ? 'scale3d(1, 1, 1)' : 'scale3d(1, -1, 1)';

    const render = (index: BigInteger) => {
      const measure = (element: any) => {
        if (element) {
          this.cache.set(index, {
            height: element.offsetHeight,
            element
          });
          this.recalculateTotalHeight();
        }
      };
      return renderer({ index, measure, scrollWindow: this.window });
    };

    return (
      <Box overflowY='scroll' ref={this.setWindow.bind(this)} onScroll={this.onScroll.bind(this)} style={{ ...style, ...{ transform } }}>
        <Box ref={this.scrollContainer} style={{ transform, width: '100%' }}>
          <Box style={{ height: `${origin === 'top' ? startgap : endgap}px` }}></Box>
          {indexesToRender.map(render)}
          <Box style={{ height: `${origin === 'top' ? endgap : startgap}px` }}></Box>
        </Box>
      </Box>
    );
  }
}
