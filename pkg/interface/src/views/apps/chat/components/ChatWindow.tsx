import React, { Component } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import _ from 'lodash';
import bigInt, { BigInteger } from 'big-integer';

import { Col } from '@tlon/indigo-react';
import {
  Patp,
  Contacts,
  Association,
  Associations,
  Group,
  Groups,
  Graph
} from '@urbit/api';

import GlobalApi from '~/logic/api/global';

import VirtualScroller from '~/views/components/VirtualScroller';

import ChatMessage, { MessagePlaceholder } from './ChatMessage';
import { UnreadNotice } from './unread-notice';
import withState from '~/logic/lib/withState';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import useGraphState from '~/logic/state/graph';

const INITIAL_LOAD = 20;
const DEFAULT_BACKLOG_SIZE = 100;
const IDLE_THRESHOLD = 64;
const MAX_BACKLOG_SIZE = 1000;

type ChatWindowProps = RouteComponentProps<{
  ship: Patp;
  station: string;
}> & {
  unreadCount: number;
  graph: Graph;
  graphSize: number;
  association: Association;
  group: Group;
  ship: Patp;
  station: any;
  api: GlobalApi;
  scrollTo?: number;
};

interface ChatWindowState {
  fetchPending: boolean;
  idle: boolean;
  initialized: boolean;
  unreadIndex: BigInteger;
}

const virtScrollerStyle = { height: '100%' };

class ChatWindow extends Component<
  ChatWindowProps,
  ChatWindowState
> {
  private virtualList: VirtualScroller | null;
  private unreadMarkerRef: React.RefObject<HTMLDivElement>;
  private prevSize = 0;

  INITIALIZATION_MAX_TIME = 100;

  constructor(props: ChatWindowProps) {
    super(props);

    this.state = {
      fetchPending: false,
      idle: true,
      initialized: false,
      unreadIndex: bigInt.zero
    };

    this.dismissUnread = this.dismissUnread.bind(this);
    this.scrollToUnread = this.scrollToUnread.bind(this);
    this.handleWindowBlur = this.handleWindowBlur.bind(this);
    this.handleWindowFocus = this.handleWindowFocus.bind(this);
    this.stayLockedIfActive = this.stayLockedIfActive.bind(this);

    this.virtualList = null;
    this.unreadMarkerRef = React.createRef();
    this.prevSize = props.graph.size;
  }

  componentDidMount() {
    this.calculateUnreadIndex();
    setTimeout(() => {
      if (this.props.scrollTo) {
        this.scrollToUnread();
      }
      this.setState({ initialized: true });
      
    }, this.INITIALIZATION_MAX_TIME);
  }

  calculateUnreadIndex() {
    const { graph, unreadCount } = this.props;
    const { state } = this;
    if(state.unreadIndex.neq(bigInt.zero)) {
      return;
    }
    const unreadIndex = graph.keys()[unreadCount];
    if (!unreadIndex || unreadCount === 0) {
      this.setState({
        unreadIndex: bigInt.zero
      });
      return;
    }
    this.setState({
      unreadIndex
    });
  }

  dismissedInitialUnread() {
    const { unreadCount, graph } = this.props;
    return this.state.unreadIndex.neq(graph.keys()?.[unreadCount]?.[0] ?? bigInt.zero)
  }

  handleWindowBlur() {
    this.setState({ idle: true });
  }

  handleWindowFocus() {
    this.setState({ idle: false });
    if (this.virtualList?.window?.scrollTop === 0) {
      this.dismissUnread();
    }
  }

  componentDidUpdate(prevProps: ChatWindowProps, prevState) {
    const { history, graph, unreadCount, graphSize, station } = this.props;

    if(this.prevSize !== graphSize) {
      this.prevSize = graphSize;
      if(this.dismissedInitialUnread() 
        && this.virtualList?.startOffset() === 0) {
        this.dismissUnread();
      }
      if(this.state.unreadIndex.eq(bigInt.zero)) {
        this.calculateUnreadIndex();
      }
    }

    if (unreadCount > prevProps.unreadCount) {
      this.calculateUnreadIndex();
    }

    if (station !== prevProps.station) {
      this.virtualList?.resetScroll();
      this.calculateUnreadIndex();
    }
  }

  stayLockedIfActive() {
    if (this.virtualList && !this.state.idle) {
      this.virtualList.resetScroll();
      this.dismissUnread();
    }
  }

  onBottomLoaded = () => {
    if(this.state.unreadIndex.eq(bigInt.zero)) {
      this.calculateUnreadIndex();
    }
  }

  scrollToUnread() {
    const { unreadIndex } = this.state;
    if (unreadIndex.eq(bigInt.zero)) {
      return;
    }

    this.virtualList?.scrollToIndex(this.state.unreadIndex);
  }

  dismissUnread() {
    const { association } = this.props;
    if (this.state.fetchPending) return;
    if (this.props.unreadCount === 0) return;
    this.props.api.hark.markCountAsRead(association, '/', 'message');
  }

  setActive = () => { 
    if(this.state.idle) {
      this.setState({ idle: false });
    }
  }

  fetchMessages = async (newer: boolean): Promise<boolean> => {
    const { api, station, graph } = this.props;
    const pageSize = 100;

    const [, , ship, name] = station.split('/');
    const expectedSize = graph.size + pageSize;
    if (newer) {
      const [index] = graph.peekLargest()!;
      await api.graph.getYoungerSiblings(
        ship,
        name,
        pageSize,
        `/${index.toString()}`
      );
      return expectedSize !== graph.size;
    } else {
      const [index] = graph.peekSmallest()!;
      await api.graph.getOlderSiblings(ship, name, pageSize, `/${index.toString()}`);
      const done = expectedSize !== graph.size;
      if(done) {
        this.calculateUnreadIndex();
      }
      return done;
    }
  }

  onScroll = ({ scrollTop, scrollHeight, windowHeight }) => {
    if (!this.state.idle && scrollTop > IDLE_THRESHOLD) {
      this.setState({ idle: true });
    }
  }


  renderer = React.forwardRef(({ index, scrollWindow }, ref) => {
    const {
      api,
      association,
      group,
      showOurContact,
      graph,
      history,
      groups,
      associations
    } = this.props;
    const { unreadMarkerRef } = this;
    const messageProps = {
      association,
      group,
      showOurContact,
      unreadMarkerRef,
      history,
      api,
      groups,
      associations
    };

    const msg = graph.get(index)?.post;
    if (!msg) return null;
    if (!this.state.initialized) {
      return (
        <MessagePlaceholder
          key={index.toString()}
          height='64px'
          index={index}
        />
      );
    }
    const isPending: boolean = 'pending' in msg && Boolean(msg.pending);
    const isLastMessage = index.eq(
      graph.peekLargest()?.[0] ?? bigInt.zero
    );
    const highlighted = false; // this.state.unreadIndex.eq(index);
    const keys = graph.keys().reverse();
    const graphIdx = keys.findIndex((idx) => idx.eq(index));
    const prevIdx = keys[graphIdx + 1];
    const nextIdx = keys[graphIdx - 1];
    const isLastRead: boolean = this.state.unreadIndex.eq(index);
    const props = {
      highlighted,
      scrollWindow,
      isPending,
      isLastRead,
      isLastMessage,
      msg,
      ...messageProps
    };

    return (
      <ChatMessage
        key={index.toString()}
        ref={ref}
        previousMsg={prevIdx && graph.get(prevIdx)?.post}
        nextMsg={nextIdx && graph.get(nextIdx)?.post}
        {...props}
      />
    );
  });

  render() {
    const {
      unreadCount,
      api,
      association,
      group,
      graph,
      history,
      groups,
      associations,
      showOurContact,
      pendingSize
    } = this.props;

    const unreadMarkerRef = this.unreadMarkerRef;
    const messageProps = {
      association,
      group,
      unreadMarkerRef,
      history,
      api,
      associations
    };
    const unreadMsg = graph.get(this.state.unreadIndex);

    // hack to force a re-render when we toggle showing contact
    const contactsModified =
      showOurContact ? 0 : 100;

    return (
      <Col height='100%' overflow='hidden' position='relative'>
        <UnreadNotice
          unreadCount={unreadCount}
          unreadMsg={
            unreadCount === 1 &&
            unreadMsg &&
            unreadMsg?.post.author === window.ship
              ? false
              : unreadMsg
          }
          dismissUnread={this.dismissUnread}
          onClick={this.scrollToUnread}
        />
        <VirtualScroller
          ref={(list) => {
            this.virtualList = list;
          }}
          offset={unreadCount}
          origin='bottom'
          style={virtScrollerStyle}
          onStartReached={this.setActive}
          onBottomLoaded={this.onBottomLoaded}
          onScroll={this.onScroll}
          data={graph}
          size={graph.size}
          pendingSize={pendingSize + contactsModified}
          id={association.resource}
          averageHeight={22}
          renderer={this.renderer}
          loadRows={this.fetchMessages}
        />
      </Col>
    );
  }
}

export default withState(ChatWindow, [
  [useGroupState, ['groups']],
  [useMetadataState, ['associations']],
  [useGraphState, ['pendingSize']]
]);
