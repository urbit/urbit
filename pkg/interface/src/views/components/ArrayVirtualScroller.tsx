import { Box, Center, LoadingSpinner } from '@tlon/indigo-react';
import BigIntArrayOrderedMap, {
  arrToString,
  stringToArr 
} from '@urbit/api/lib/BigIntArrayOrderedMap';
import bigInt, { BigInteger } from 'big-integer';
import _ from 'lodash';
import normalizeWheel from 'normalize-wheel';
import React, { Component, SyntheticEvent, useCallback } from 'react';
import styled from 'styled-components';
import { IS_IOS } from '~/logic/lib/platform';
import { VirtualContext } from '~/logic/lib/virtualContext';
import { clamp } from '~/logic/lib/util';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface RendererProps {
  index: BigInteger[];
  scrollWindow: any;
  ref: (el: HTMLElement | null) => void;
}

export { arrToString, stringToArr };

export function indexEqual(a: BigInteger[], b: BigInteger[]) {
  let aLen = a.length;
  let bLen = b.length;

  if (aLen === bLen) {
    let i = 0;
    while (i < aLen && i < bLen) {
      if (a[i].eq(b[i])) {
        if (i === aLen - 1) {
          return true;
        }
        i++;
      } else {
        return false;
      }
    }
  }

  return false;
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
  data: BigIntArrayOrderedMap<T>;
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
  /**
   * Callback to execute when finished loading from start
  */
  onBottomLoaded?: () => void;
}

interface VirtualScrollerState<T> {
  visibleItems: BigInteger[][];
  scrollbar: number;
  loaded: {
    top: boolean;
    bottom: boolean;
  }
}

type LogLevel = 'scroll' | 'network' | 'bail' | 'reflow';
const logLevel = ['network', 'bail', 'scroll', 'reflow'] as LogLevel[];

const log = (level: LogLevel, message: string) => {
  if(logLevel.includes(level)) {
    console.log(`[${level}]: ${message}`);
  }
};

const ZONE_SIZE = IS_IOS ? 20 : 80;

// nb: in this file, an index refers to a BigInteger[] and an offset refers to a
// number used to index a listified BigIntArrayOrderedMap

/**
 * A virtualscroller for a `BigIntArrayOrderedMap`.
 *
 * VirtualScroller does not clean up or reset itself, so please use `key`
 * to ensure a new instance is created for each BigIntArrayOrderedMap
 */
export default class ArrayVirtualScroller<T> extends Component<VirtualScrollerProps<T>, VirtualScrollerState<T>> {
  /**
   * A reference to our scroll container
   */
  window: HTMLDivElement | null = null;
  /**
   * A map of child refs, used to calculate scroll position
   */
  private childRefs = new Map<string, HTMLElement>();
  /**
   * A set of child refs which have been unmounted
   */
  private orphans = new Set<string>();
  /**
   *  If saving, the bottommost visible element that we pin our scroll to
   */
  private savedIndex: BigInteger[] | null = null;
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

  scrollLocked = true;

  private pageSize = 50;

  private pageDelta = 15;

  private scrollRef: HTMLElement | null = null;

  private cleanupRefInterval: NodeJS.Timeout | null = null;

  constructor(props: VirtualScrollerProps<T>) {
    super(props);
    this.state = {
      visibleItems: [],
      scrollbar: 0,
      loaded: {
        top: false,
        bottom: false
      }
    };

    this.updateVisible = this.updateVisible.bind(this);

    this.invertedKeyHandler = this.invertedKeyHandler.bind(this);
    this.onScroll = IS_IOS ? _.debounce(this.onScroll.bind(this), 200) : this.onScroll.bind(this);
    this.scrollKeyMap = this.scrollKeyMap.bind(this);
    this.setWindow = this.setWindow.bind(this);
    this.restore = this.restore.bind(this);
    this.startOffset = this.startOffset.bind(this);
  }

  componentDidMount() {
    this.updateVisible(0);
    this.loadTop();
    this.loadBottom();
    this.cleanupRefInterval = setInterval(this.cleanupRefs, 5000);
  }

  cleanupRefs = () => {
    if(this.saveDepth > 0) {
      return;
    }
    [...this.orphans].forEach((o) => {
      this.childRefs.delete(o);
    });
    this.orphans.clear();
  };

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
    const result = ((unloaded + loaded) / totalpages) * this.window.offsetHeight;
    this.scrollRef.style[this.props.origin] = `${result}px`;
  }, 50);

  componentDidUpdate(prevProps: VirtualScrollerProps<T>, _prevState: VirtualScrollerState<T>) {
    const { size, data, offset, pendingSize } = this.props;

    if(size !== prevProps.size || pendingSize !== prevProps.pendingSize) {
      if((this.window?.scrollTop ?? 0) < ZONE_SIZE) {
        this.scrollLocked = true;
        this.updateVisible(0);
        this.resetScroll();
      }
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.invertedKeyHandler);
    if(this.cleanupRefInterval) {
      clearInterval(this.cleanupRefInterval);
    }
    this.cleanupRefs();
    this.childRefs.clear();
  }

  startOffset() {
    const { data } = this.props;
    const startIndex = this.state.visibleItems?.[0];
    if(!startIndex) {
      return 0;
    }
    const dataList = Array.from(data);
    const offset = dataList.findIndex(([i]) => indexEqual(i, startIndex));
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

    const { data } = this.props;
    const visibleItems = data.keys().slice(newOffset, newOffset + this.pageSize);

    this.save();

    this.setState({
      visibleItems
    });
    requestAnimationFrame(() => {
      this.restore();
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
    this.pageSize = Math.floor(element.offsetHeight / Math.floor(averageHeight / 2));
    this.pageDelta = Math.floor(this.pageSize / 4);

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
  loadTop = _.throttle(() => this.loadRows(false), 100);
  loadBottom = _.throttle(() => this.loadRows(true), 100);

  loadRows = async (newer: boolean) => {
    const dir = newer ? 'bottom' : 'top';
    if(this.state.loaded[dir]) {
      return;
    }
    log('network', `loading more at ${dir}`);
    const done = await this.props.loadRows(newer);
    if(done) {
      this.setState({
        loaded: {
          ...this.state.loaded,
          [dir]: done
        }
      });
      if(newer && this.props.onBottomLoaded) {
        this.props.onBottomLoaded();
      }
    }
  };

  onScroll(event: SyntheticEvent<HTMLElement>) {
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

    const scrollEnd = scrollTop + windowHeight;

    if (scrollTop < ZONE_SIZE) {
      log('scroll', `Entered start zone ${scrollTop}`);
      if (startOffset === 0) {
        onStartReached && onStartReached();
        this.scrollLocked = true;
      }

      const newOffset = 
        clamp(startOffset - this.pageDelta, 0, this.props.data.size - this.pageSize);
      if(newOffset < 10) {
        this.loadBottom();
      }

      if(newOffset !== startOffset) {
        this.updateVisible(newOffset);
      }
    } else if (scrollTop + windowHeight >= scrollHeight - ZONE_SIZE) {
      this.scrollLocked = false;
      log('scroll', `Entered end zone ${scrollTop}`);

      const newOffset = 
        clamp(startOffset + this.pageDelta, 0, this.props.data.size - this.pageSize);
      if (onEndReached && startOffset === 0) {
        onEndReached();
      }

      if((newOffset + (3 * this.pageSize) > this.props.data.size)) {
        this.loadTop();
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
    if(this.scrollLocked) {
        this.resetScroll();
        requestAnimationFrame(() => {
          this.savedIndex = null;
          this.savedDistance = 0;
          this.saveDepth--;
        });
      return;
    }

    const ref = this.childRefs.get(arrToString(this.savedIndex));
    if(!ref) {
      return;
    }

    const newScrollTop = this.props.origin === 'top'
      ? this.savedDistance + ref.offsetTop
      : this.window.scrollHeight - ref.offsetTop - this.savedDistance;

    this.window.scrollTo(0, newScrollTop);
    requestAnimationFrame(() => {
        this.savedIndex = null;
        this.savedDistance = 0;
        this.saveDepth--;
      });
  }

  scrollToIndex = (index: BigInteger[]) => {
    let ref = this.childRefs.get(arrToString(index));
    if(!ref) {
      const offset = [...this.props.data].findIndex(([idx]) => indexEqual(idx, index));
      if(offset === -1) {
        return;
      }
      this.scrollLocked = false;
      this.updateVisible(Math.max(offset - this.pageDelta, 0));
      requestAnimationFrame(() => {
        ref = this.childRefs.get(arrToString(index));
        requestAnimationFrame(() => {
          this.savedIndex = null;
          this.savedDistance = 0;
          this.saveDepth = 0;
        });

        ref?.scrollIntoView({ block: 'center' });
      });
    } else {
      ref?.scrollIntoView({ block: 'center' });
      requestAnimationFrame(() => {
        this.savedIndex = null;
        this.savedDistance = 0;
        this.saveDepth = 0;
      });
    }
  };

  save() {
    if(!this.window || this.savedIndex) {
      return;
    }
    if(this.saveDepth !== 0) {
      return;
    }

    log('scroll', 'saving...');

    this.saveDepth++;
    const { visibleItems } = this.state;

    let bottomIndex = visibleItems[visibleItems.length - 1];
    const { scrollTop, scrollHeight } = this.window;
    const topSpacing = this.props.origin === 'top' ? scrollTop : scrollHeight - scrollTop;
    const items = this.props.origin === 'top' ? visibleItems : [...visibleItems].reverse();
    items.forEach((index) => {
      const el = this.childRefs.get(arrToString(index));
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
      log('bail', 'no index found');
      return;
    }

    this.savedIndex = bottomIndex;
    const ref = this.childRefs.get(arrToString(bottomIndex))!;
    if(!ref) {
      this.saveDepth--;
      log('bail', 'missing ref');
      return;
    }
    const { offsetTop } = ref;
    this.savedDistance = topSpacing - offsetTop;
  }

  // disabled until we work out race conditions with loading new nodes
  shiftLayout = { save: () => {}, restore: () => {} };

  setRef = (element: HTMLElement | null, index: BigInteger[]) => {
    if(element) {
      this.childRefs.set(arrToString(index), element);
      this.orphans.delete(arrToString(index));
    } else {
      this.orphans.add(arrToString(index));
    }
  }

  render() {
    const {
      visibleItems
    } = this.state;

    const {
      origin = 'top',
      renderer,
      style
    } = this.props;

    const isTop = origin === 'top';

    const transform = isTop ? 'scale3d(1, 1, 1)' : 'scale3d(1, -1, 1)';
    const children = isTop ? visibleItems : [...visibleItems].reverse();

    const atStart =
      indexEqual(
        (this.props.data.peekLargest()?.[0] ?? [bigInt.zero]),
        (visibleItems?.[0] || [bigInt.zero])
      );

    const atEnd =
      indexEqual(
        (this.props.data.peekSmallest()?.[0] ?? [bigInt.zero]),
        (visibleItems?.[visibleItems.length - 1] || [bigInt.zero])
      );

    return (
      <>
        {!IS_IOS && (<Box borderRadius={3} top ={isTop ? '0' : undefined}
bottom={!isTop ? '0' : undefined} ref={(el) => {
 this.scrollRef = el;
}}
right={0} height="50px"
position="absolute" width="4px"
backgroundColor="lightGray"
                     />)}

      <ScrollbarLessBox overflowY='scroll' ref={this.setWindow} onScroll={this.onScroll} style={{ ...style, ...{ transform }, 'WebkitOverflowScrolling': 'auto' }}>
        <Box style={{ transform, width: 'calc(100% - 4px)' }}>
          {(isTop ? !atStart : !atEnd) && (
          <Center height={5}>
            <LoadingSpinner />
          </Center>
          )}
          <VirtualContext.Provider value={this.shiftLayout}>
            {children.map(index => (
              <VirtualChild
                key={arrToString(index)}
                setRef={this.setRef}
                index={index}
                scrollWindow={this.window}
                renderer={renderer}
              />
            ))}
          </VirtualContext.Provider>
          {(!isTop ? !atStart : !atEnd) &&
            (<Center height={5}>
              <LoadingSpinner />
            </Center>)}
        </Box>
      </ScrollbarLessBox>
    </>
    );
  }
}

interface VirtualChildProps {
  index: BigInteger[];
  scrollWindow: any;
  setRef: (el: HTMLElement | null, index: BigInteger[]) => void;
  renderer: (p: RendererProps) => JSX.Element | null;
}

function VirtualChild(props: VirtualChildProps) {
  const { setRef, renderer: Renderer, ...rest } = props;

  const ref = useCallback((el: HTMLElement | null) => {
    setRef(el, props.index);
  //  VirtualChild should always be keyed on the index, so the index should be
  //  valid for the entire lifecycle of the component, hence no dependencies
  }, []);

  return <Renderer ref={ref} {...rest} />;
}

