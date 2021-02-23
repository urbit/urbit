import React, { Component } from 'react';
import _ from 'lodash';
import normalizeWheel from 'normalize-wheel';
import bigInt, { BigInteger } from 'big-integer';

import { Box } from '@tlon/indigo-react';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';

interface RendererProps {
  index: BigInteger;
  shiftLayout: {
    save: () => void;
    restore: () => void;
  }
  scrollWindow: any;
  ref: (el: HTMLElement | null) => void;
}
const PAGE_DELTA = 20;
const PAGE_SIZE = 60;

interface VirtualScrollerProps<T> {
  origin: 'top' | 'bottom';
  loadRows(newer: boolean): Promise<boolean>;
  data: BigIntOrderedMap<T>;
  id: string;
  renderer: (props: RendererProps) => JSX.Element | null;
  onStartReached?(): void;
  onEndReached?(): void;
  size: number;
  totalSize: number;
  
  onCalculateVisibleItems?(visibleItems: BigIntOrderedMap<T>): void;
  onScroll?({ scrollTop, scrollHeight, windowHeight }): void;
  style?: any;
}

interface VirtualScrollerState<T> {
  startgap: number | undefined;
  visibleItems: BigIntOrderedMap<T>;
  endgap: number | undefined;
  totalHeight: number;
  averageHeight: number;
}

// nb: in this file, an index refers to a BigInteger and an offset refers to a
// number used to index a listified BigIntOrderedMap

export default class VirtualScroller<T> extends Component<VirtualScrollerProps<T>, VirtualScrollerState<T>> {
  /**
   * A reference to our scroll container 
   */
  private window: HTMLDivElement | null = null;
  /**
   * A map of child refs, used to calculate scroll position
   */
  private childRefs = new BigIntOrderedMap<HTMLElement>();
  /**
   *  If saving, the bottommost visible element that we pin our scroll to
   */
  private savedIndex: BigInteger | null = null;
  /**
   *  If saving, the distance between the top of `this.savedEl` and the bottom
   *  of the screen
   */
  private savedDistance = 0;

  /**
   *  If saving, the number of requested saves. If several images are loading
   *  at once, we save the scroll pos the first time we see it and restore
   *  once the number of requested saves is zero
   */
  private saveDepth = 0;

  private isUpdating = false;

  private scrollLocked = true;

  private dataEdges: [BigInteger, BigInteger] = [bigInt.zero, bigInt.zero];

  private loaded = {
    top: false,
    bottom: false
  };

  private dirtied : 'top' | 'bottom' | null = null;

  constructor(props: VirtualScrollerProps<T>) {
    super(props);
    this.state = {
      startgap: props.origin === 'top' ? 0 : undefined,
      visibleItems: new BigIntOrderedMap(),
      endgap: props.origin === 'bottom' ? 0 : undefined,
      totalHeight: 0,
      averageHeight: 130,
    };

    this.updateVisible = this.updateVisible.bind(this);
    this.invertedKeyHandler = this.invertedKeyHandler.bind(this);
    this.onScroll = this.onScroll.bind(this)
    this.scrollKeyMap = this.scrollKeyMap.bind(this);
    window.restore = () => this.restore();
    window.save = () => this.save();
  }

  componentDidMount() {
    this.updateDataEdges();
    this.updateVisible(0);
  }

  componentDidUpdate(prevProps: VirtualScrollerProps<T>, _prevState: VirtualScrollerState<T>) {
    const { id, size, data } = this.props;
    const { visibleItems } = this.state;
    if(id !== prevProps.id) {
      this.resetScroll();
      this.updateVisible(0);
    } else if(size !== prevProps.size) {
      const index = visibleItems.peekSmallest()?.[0]!;
      const newOffset = [...data].findIndex(([i]) => i.eq(index));
      
      if(this.scrollLocked && this.dataEdges[1].neq(data.peekLargest()?.[0]!)) {
        console.log('locking');
        this.updateVisible(0);
        return;
      } else if(this.scrollLocked || newOffset === -1) {
        console.log('resetting');
        this.resetScroll();
      }
      this.updateDataEdges();
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.invertedKeyHandler);
  }

  updateDataEdges() {
    const { data } = this.props;
    const small = data.peekSmallest()?.[0]!;
    const large = data.peekLargest()?.[0]!;

    this.dataEdges = [small, large];
  }

  startOffset() {
    const startIndex = this.state.visibleItems.peekLargest()?.[0]!;
    const offset = [...this.props.data].findIndex(([i]) => i.eq(startIndex))
    if(offset === -1) {
      throw new Error("a");
    }
    return offset;
  }

  /**
   *  Updates the `startOffset` and adjusts visible items accordingly.
   *  Saves the scroll positions before repainting and restores it afterwards
   */
  updateVisible(newOffset: number) {
    if (!this.window || this.isUpdating) {
      return;
    }
    this.isUpdating = true;

    const { data, onCalculateVisibleItems } = this.props;
    const visibleItems = new BigIntOrderedMap<any>(
      [...data].slice(newOffset, newOffset + PAGE_SIZE)
    );

    this.save();

    onCalculateVisibleItems ? onCalculateVisibleItems(visibleItems) : null;
    this.setState({
      visibleItems,
    }, () => {
      requestAnimationFrame(() => {
        this.restore();
        this.isUpdating = false;
      });
    });
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

  setWindow(element) {
    if (!element)
      return;
    if (this.window) {
      if (this.window.isSameNode(element)) {
        return;
      } else {
        window.removeEventListener('keydown', this.invertedKeyHandler);
      }
    }

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

  resetScroll() {
    if (!this.window) {
      return;
    }
    this.window.scrollTop = 0;
    this.savedIndex = null;
    this.savedDistance = 0;
    this.saveDepth = 0;
  }

  async loadRows(newer: boolean) {
    const dir = newer ? 'bottom' : 'top';
    if(this.loaded[dir]) {
      return;
    }
    const done = await this.props.loadRows(newer);
    if(done) {
      this.dirtied = dir;
      this.loaded[dir] = true;
    }
  }

  onScroll(event: UIEvent) {
    if(!this.window || this.savedIndex) {
      // bail if we're going to adjust scroll anyway
      return;
    }

    const { onStartReached, onEndReached } = this.props;
    const windowHeight = this.window.offsetHeight;
    const { scrollTop, scrollHeight } = this.window;

    const startOffset = this.startOffset();
    if (scrollTop < 20) {
      if (onStartReached) {
        onStartReached();
      }
      const newOffset = Math.max(0, startOffset - PAGE_DELTA);
      if(newOffset < 10) {
        setTimeout(() => this.loadRows(true));
      }

      if(newOffset === 0) {
        this.scrollLocked = true;
      }
      if(newOffset !== startOffset) {
        this.updateVisible(newOffset);
      }
    } 
    else if (scrollTop + windowHeight >= scrollHeight - 100) {
      if (onEndReached) {
        onEndReached();
      }

      const newOffset = Math.min(startOffset + PAGE_DELTA, this.props.data.size - PAGE_SIZE);
      if((newOffset + 10 < this.props.data.size - PAGE_SIZE)) {
        setTimeout(() => this.loadRows(false));
      }

      if(newOffset !== startOffset) {
        this.updateVisible(newOffset);
      }
    } else {
      this.scrollLocked = false;
    }
  }

  restore() {
    if(!this.window || !this.savedIndex) {
      return;
    }
    this.saveDepth--;
    if(this.saveDepth !== 0) {
      //console.log('multiple restores');
      return;
    }
  
    const { offsetTop } = this.childRefs.get(this.savedIndex)!;
    const newScrollTop = this.window.scrollHeight - offsetTop - this.savedDistance;
    
    this.window.scrollTop = newScrollTop;
    this.savedIndex = null;
    this.savedDistance = 0;
  }

  save() {
    if(!this.window || this.savedIndex) {
      return;
    }
    this.saveDepth++;
    if(this.saveDepth !== 1) {
      //console.log(new Error().stack);
      //console.log('multiple saves');
      return;
    }

    let bottomIndex: BigInteger | null = null;
    const { scrollTop, scrollHeight } = this.window;
    const topSpacing = scrollHeight - scrollTop;
    [...Array.from(this.state.visibleItems)].reverse().forEach(([index, datum]) => {
      const idxTop = this.childRefs.get(index)!.offsetTop;
      if(idxTop < topSpacing) {
        bottomIndex = index;
      }
    });

    if(!bottomIndex) {
      console.log('weird case');
      // weird, shouldn't really happen
      return;
    }

    this.savedIndex = bottomIndex;
    const { offsetTop } = this.childRefs.get(bottomIndex)!;
    this.savedDistance = topSpacing - offsetTop
  }

  shiftLayout = { save: this.save.bind(this), restore: this.restore.bind(this) };

  setRef = (index: BigInteger) => (element: HTMLElement | null) => {
    if(element) {
      this.childRefs.set(index, element);
    } else {
      this.childRefs.delete(index);
    }
  }

  renderItem = (index: BigInteger) => {
    const ref = this.setRef(index);
    return this.props.renderer({
      index,
      ref,
      shiftLayout: this.shiftLayout,
      scrollWindow: this.window,
    });
  };



  render() {
    const {
      startgap,
      endgap,
      visibleItems
    } = this.state;

    const {
      origin = 'top',
      renderer,
      style,
    } = this.props;

    const indexesToRender = origin === 'top' ? visibleItems.keys() : visibleItems.keys().reverse();

    const transform = origin === 'top' ? 'scale3d(1, 1, 1)' : 'scale3d(1, -1, 1)';

    
    return (
      <Box overflowY='scroll' ref={this.setWindow.bind(this)} onScroll={this.onScroll} style={{ ...style, ...{ transform } }}>
        <Box style={{ transform, width: '100%' }}>
          <Box style={{ height: `${origin === 'top' ? startgap : endgap}px` }}></Box>
          {indexesToRender.map(this.renderItem)}
          <Box style={{ height: `${origin === 'top' ? endgap : startgap}px` }}></Box>
        </Box>
      </Box>
    );
  }
}
