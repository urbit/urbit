import React, { Component, useCallback } from 'react';
import _ from 'lodash';
import normalizeWheel from 'normalize-wheel';
import bigInt, { BigInteger } from 'big-integer';
import styled from 'styled-components';

import { Box, LoadingSpinner, Row, Center } from '@tlon/indigo-react';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import {VirtualContext} from '~/logic/lib/virtualContext';
import { IS_IOS } from '~/logic/lib/platform';
const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;


interface RendererProps {
  index: BigInteger;
  scrollWindow: any;
  ref: (el: HTMLElement | null) => void;
}

interface VirtualScrollerProps<T> {
  /**
   * Start scroll from
   */
  origin: 'top' | 'bottom';
  /**
   * Load more of the graph
   *
   * @returns boolean whether or not the graph is now fully loaded
   */
  loadRows(newer: boolean): Promise<boolean>;
  /**
   * The data to iterate over
   */
  data: BigIntOrderedMap<T>;
  /**
   * The component to render the items
   *
   * @remarks
   *
   * This component must be referentially stable, so either use `useCallback` or
   * a instance method. It must also forward the DOM ref from its root DOM node
   */
  renderer: (props: RendererProps) => JSX.Element | null;
  onStartReached?(): void;
  onEndReached?(): void;
  size: number;
  pendingSize: number;
  totalSize: number;
  /**
   * Average height of a single rendered item
   *
   * @remarks
   * This is used primarily to calculate how many items should be onscreen. If
   * size is variable, err on the lower side.
   */
  averageHeight: number;
  /**
   * The offset to begin rendering at, on load.
   *
   * @remarks
   * This is only looked up once, on component creation. Subsequent changes to
   * this prop will have no effect
   */
  offset: number;
  style?: any;
}

interface VirtualScrollerState<T> {
  visibleItems: BigIntOrderedMap<T>;
  scrollbar: number;
}

type LogLevel = 'scroll' | 'network' | 'bail' | 'reflow';
let logLevel = ['bail', 'scroll', 'reflow'] as LogLevel[];

const log = (level: LogLevel, message: string) => {
  if(logLevel.includes(level)) {
    console.log(`[${level}]: ${message}`);
  }

}

const ZONE_SIZE = IS_IOS ? 10 : 40;


// nb: in this file, an index refers to a BigInteger and an offset refers to a
// number used to index a listified BigIntOrderedMap

/**
 * A virtualscroller for a `BigIntOrderedMap`.
 *
 * VirtualScroller does not clean up or reset itself, so please use `key`
 * to ensure a new instance is created for each BigIntOrderedMap
 */
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

  private scrollLocked = true;

  private pageSize = 50;

  private pageDelta = 15;

  private scrollRef: HTMLElement | null = null;

  private loaded = {
    top: false,
    bottom: false
  };

  constructor(props: VirtualScrollerProps<T>) {
    super(props);
    this.state = {
      visibleItems: new BigIntOrderedMap(),
      scrollbar: 0
    };

    this.updateVisible = this.updateVisible.bind(this);

    this.invertedKeyHandler = this.invertedKeyHandler.bind(this);
    this.onScroll = IS_IOS ? _.debounce(this.onScroll.bind(this), 400) : this.onScroll.bind(this);
    this.scrollKeyMap = this.scrollKeyMap.bind(this);
    this.setWindow = this.setWindow.bind(this);
  }

  componentDidMount() {
    if(this.props.size < 100) {
      this.loaded.top = true;
      this.loaded.bottom = true;
    }

    this.updateVisible(0);
    this.resetScroll();
    this.loadRows(false);
  }

  // manipulate scrollbar manually, to dodge change detection
  updateScroll = IS_IOS ? () => {} : _.throttle(() => {
    if(!this.window || !this.scrollRef) {
      return;
    }
    const { scrollTop, scrollHeight, offsetHeight } = this.window;

    const unloaded = (this.startOffset() / this.pageSize);
    const totalpages = this.props.size / this.pageSize;

    const loaded = (scrollTop / scrollHeight);
    const total = unloaded +  loaded;
    const result = ((unloaded + loaded) / totalpages) *this.window.offsetHeight;
    this.scrollRef.style[this.props.origin] = `${result}px`;
  }, 50);



  componentDidUpdate(prevProps: VirtualScrollerProps<T>, _prevState: VirtualScrollerState<T>) {
    const { id, size, data, offset, pendingSize } = this.props;
    const { visibleItems } = this.state;

    if(size !== prevProps.size || pendingSize !== prevProps.pendingSize) {
      if(this.scrollLocked) {
        this.updateVisible(0);
        this.resetScroll();

      }
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.invertedKeyHandler);
  }

  startOffset() {
    const startIndex = this.state?.visibleItems?.peekLargest()?.[0];
    if(!startIndex) {
      return 0;
    }
    const offset = [...this.props.data].findIndex(([i]) => i.eq(startIndex))
    if(offset === -1) {
      // TODO: revisit when we remove nodes for any other reason than
      // pending indices being removed
      return 0;
    }
    return offset;
  }

  /**
   *  Updates the `startOffset` and adjusts visible items accordingly.
   *  Saves the scroll positions before repainting and restores it afterwards
   */
  updateVisible(newOffset: number) {
    if (!this.window) {
      return;
    }
    log('reflow', `from: ${this.startOffset()} to: ${newOffset}`);

    const { data, onCalculateVisibleItems } = this.props;
    const visibleItems = new BigIntOrderedMap<any>(
      [...data].slice(newOffset, newOffset + this.pageSize)
    );

    this.save();

    this.setState({
      visibleItems,
    }, () => {
      requestAnimationFrame(() => {
        this.restore();
        requestAnimationFrame(() => {

        });
      });
    });
  }

  scrollKeyMap(): Map<string, number> {
    return new Map([
      ['ArrowUp', this.props.averageHeight],
      ['ArrowDown', this.props.averageHeight * -1],
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
    console.log('resetting window');
    this.save();

    if (this.window) {
      if (this.window.isSameNode(element)) {
        return;
      } else {
        window.removeEventListener('keydown', this.invertedKeyHandler);
      }
    }
    const { averageHeight } = this.props;

    this.window = element;
    this.pageSize = Math.floor(element.offsetHeight / Math.floor(averageHeight / 5.5));
    this.pageDelta = Math.floor(this.pageSize / 3);
    if (this.props.origin === 'bottom') {
       element.addEventListener('wheel', (event) => {
        event.preventDefault();
        const normalized = normalizeWheel(event);
        element.scrollBy(0, normalized.pixelY * -1);
        return false;
      }, { passive: false });

      window.addEventListener('keydown', this.invertedKeyHandler, { passive: false });
    }
    this.restore();
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

  loadRows = _.throttle(async (newer: boolean) => {
    const dir = newer ? 'bottom' : 'top';
    if(this.loaded[dir]) {
      return;
    }
    log('network', `loading more at ${dir}`);
    const done = await this.props.loadRows(newer);
    if(done) {
      this.loaded[dir] = true;
    }
  }, 100);

  onScroll(event: UIEvent) {
    this.updateScroll();
    if(!this.window) {
      // bail if we're going to adjust scroll anyway
      return;
    }
    if(this.saveDepth > 0) {
      log('bail', 'deep scroll queue');
      return;
    }
    const { onStartReached, onEndReached } = this.props;
    const windowHeight = this.window.offsetHeight;
    const { scrollTop, scrollHeight } = this.window;

    const startOffset = this.startOffset();
    if (scrollTop < ZONE_SIZE) {
      log('scroll', `Entered start zone ${scrollTop}`);
      if (startOffset === 0 && onStartReached) {
        onStartReached();
      }
      const newOffset = Math.max(0, startOffset - this.pageDelta);
      if(newOffset < 10) {
        this.loadRows(true);
      }

      if(newOffset === 0) {
        this.scrollLocked = true;
      }
      if(newOffset !== startOffset) {
        this.updateVisible(newOffset);
      }
    }
    else if (scrollTop + windowHeight >= scrollHeight - ZONE_SIZE) {
      this.scrollLocked = false;
      log('scroll', `Entered end zone ${scrollTop}`);

      const newOffset = Math.min(startOffset + this.pageDelta, this.props.data.size - this.pageSize);
      if (onEndReached && startOffset === 0) {
        onEndReached();
      }

      if((newOffset + (3 * this.pageSize) > this.props.data.size)) {
        this.loadRows(false)
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
    if(this.saveDepth !== 1) {
      log('bail', 'Deep restore');
      return;
    }

    const ref = this.childRefs.get(this.savedIndex)!;
    const newScrollTop = this.window.scrollHeight - ref.offsetTop - this.savedDistance;

    this.window.scrollTo(0, newScrollTop);
    requestAnimationFrame(() => {
      this.savedIndex = null;
      this.savedDistance = 0;
      this.saveDepth--;
    });
  }

  scrollToIndex = (index: BigInteger) => {
    let ref = this.childRefs.get(index);
    if(!ref) {
      const offset = [...this.props.data].findIndex(([idx]) => idx.eq(index));
      if(offset === -1) {
        return;
      }
      this.updateVisible(Math.max(offset - this.pageDelta, 0));
      requestAnimationFrame(() => {
        ref = this.childRefs.get(index);
        this.savedIndex = null;
        this.savedDistance = 0;
        this.saveDepth = 0;

        ref?.scrollIntoView({ block: 'center' });
      });
    } else {
      this.savedIndex = null;
      this.savedDistance = 0;
      this.saveDepth = 0;

      ref?.scrollIntoView({ block: 'center' });
    }
  };

  save() {
    if(!this.window || this.savedIndex) {
      return;
    }
    this.saveDepth++;
    if(this.saveDepth !== 1) {
      console.log('bail', 'deep save');
      return;
    }

    let bottomIndex: BigInteger | null = null;
    const { scrollTop, scrollHeight } = this.window;
    const topSpacing = scrollHeight - scrollTop;
    [...Array.from(this.state.visibleItems)].reverse().forEach(([index, datum]) => {
      const el = this.childRefs.get(index);
      if(!el) {
        return;
      }
      const { offsetTop } = el;
      if(offsetTop < topSpacing) {
        bottomIndex = index;
      }
    });

    if(!bottomIndex) {
      // weird, shouldn't really happen
      this.saveDepth--;
      return;
    }

    this.savedIndex = bottomIndex;
    const ref = this.childRefs.get(bottomIndex)!;
    const { offsetTop } = ref;
    this.savedDistance = topSpacing - offsetTop
  }

  shiftLayout = { save: this.save.bind(this), restore: this.restore.bind(this) };

  setRef = (element: HTMLElement | null, index: BigInteger) => {
    if(element) {
      this.childRefs.set(index, element);
    } else {
      setTimeout(() => {
        this.childRefs.delete(index);
      });
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
      renderer,
      style,
    } = this.props;

    const isTop = origin === 'top';

    const indexesToRender = isTop ? visibleItems.keys() : visibleItems.keys().reverse();

    const transform = isTop ? 'scale3d(1, 1, 1)' : 'scale3d(1, -1, 1)';


    const atStart = (this.props.data.peekLargest()?.[0] ?? bigInt.zero).eq(visibleItems.peekLargest()?.[0] || bigInt.zero);
    const atEnd = this.loaded.top;

    return (
      <>
        {!IS_IOS && (<Box borderRadius="3" top ={isTop ? "0" : undefined} bottom={!isTop ? "0" : undefined} ref={el => { this.scrollRef = el; }} right="0" height="50px" position="absolute" width="4px" backgroundColor="lightGray" />)}

      <ScrollbarLessBox overflowY='scroll' ref={this.setWindow} onScroll={this.onScroll} style={{ ...style, ...{ transform }, "WebkitOverflowScrolling": "auto" }}>
        <Box style={{ transform, width: 'calc(100% - 4px)' }}>
          {(isTop ? !atStart : !atEnd) && (<Center height="5">
            <LoadingSpinner />
          </Center>)}
          <VirtualContext.Provider value={this.shiftLayout}>
            {indexesToRender.map(index => (
              <VirtualChild
                key={index.toString()}
                setRef={this.setRef}
                index={index}
                scrollWindow={this.window}
                renderer={renderer}
              />
            ))}
          </VirtualContext.Provider>
          {(!isTop ? !atStart : !atEnd) &&
            (<Center height="5">
              <LoadingSpinner />
            </Center>)}
        </Box>
      </ScrollbarLessBox>
    </>
    );
  }
}

interface VirtualChildProps {
  index: BigInteger;
  scrollWindow: any;
  setRef: (el: HTMLElement | null, index: BigInteger) => void;
  renderer: (p: RendererProps) => JSX.Element | null;
}

function VirtualChild(props: VirtualChildProps) {
  const { setRef, renderer: Renderer, ...rest } = props;

  const ref = useCallback((el: HTMLElement | null) => {
    setRef(el, props.index);
  }, [setRef, props.index])

  return (<Renderer ref={ref} {...rest} />);
};

