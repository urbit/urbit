import React, { Component } from "react";
import { RouteComponentProps } from "react-router-dom";
import _ from "lodash";
import bigInt, { BigInteger } from 'big-integer';

import { Col } from '@tlon/indigo-react';

import GlobalApi from "~/logic/api/global";
import { Patp, Path } from "~/types/noun";
import { Contacts } from "~/types/contact-update";
import { Association, Associations } from "~/types/metadata-update";
import { Group, Groups } from "~/types/group-update";
import { Envelope, IMessage } from "~/types/chat-update";
import { Graph } from "~/types";

import VirtualScroller from "~/views/components/VirtualScroller";

import ChatMessage, { MessagePlaceholder } from './ChatMessage';
import { UnreadNotice } from "./unread-notice";

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
  contacts: Contacts;
  association: Association;
  group: Group;
  ship: Patp;
  station: any;
  api: GlobalApi;
  scrollTo?: number;
  associations: Associations;
  groups: Groups;
}

interface ChatWindowState {
  fetchPending: boolean;
  idle: boolean;
  initialized: boolean;
  unreadIndex: BigInteger;
}

export default class ChatWindow extends Component<ChatWindowProps, ChatWindowState> {
  private virtualList: VirtualScroller | null;
  private unreadMarkerRef: React.RefObject<HTMLDivElement>;
  private prevSize = 0;
  private loadedNewest = false;
  private loadedOldest = false;

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
    this.dismissIfLineVisible = this.dismissIfLineVisible.bind(this);

    this.virtualList = null;
    this.unreadMarkerRef = React.createRef();
    this.prevSize = props.graph.size;
  }

  componentDidMount() {
    this.calculateUnreadIndex();
    this.virtualList?.calculateVisibleItems();
    window.addEventListener('blur', this.handleWindowBlur);
    window.addEventListener('focus', this.handleWindowFocus);
    setTimeout(() => {
      if(this.props.scrollTo) {
        this.scrollToUnread();
      }

      this.setState({ initialized: true });
    }, this.INITIALIZATION_MAX_TIME);
  }

  componentWillUnmount() {
    window.removeEventListener('blur', this.handleWindowBlur);
    window.removeEventListener('focus', this.handleWindowFocus);
  }

  calculateUnreadIndex() {
    const { graph, unreadCount } = this.props;
    const unreadIndex = graph.keys()[unreadCount];
    if(!unreadIndex || unreadCount === 0) {
      this.setState({
        unreadIndex: bigInt.zero
      });
      return;
    }
    this.setState({
      unreadIndex
    })
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
    const { history, graph, unreadCount, station } = this.props;

    if (graph.size !== prevProps.graph.size && this.state.fetchPending) {
      this.setState({ fetchPending: false });
    }

    if (unreadCount > prevProps.unreadCount && this.state.idle) {
      this.calculateUnreadIndex();
    }


    if(this.prevSize !== graph.size) {
      if(this.state.unreadIndex.eq(bigInt.zero)) {
        this.calculateUnreadIndex();
        this.scrollToUnread();
      }
      this.prevSize = graph.size;
      this.virtualList?.calculateVisibleItems();
      this.stayLockedIfActive();
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

  scrollToUnread() {
    const { unreadIndex } = this.state;
    if(unreadIndex.eq(bigInt.zero)) {
      return;
    }

    this.virtualList?.scrollToData(unreadIndex);
  }

  dismissUnread() {
    const { association } = this.props;
    if (this.state.fetchPending) return;
    if (this.props.unreadCount === 0) return;
    this.props.api.hark.markCountAsRead(association, '/', 'message');
    this.props.api.hark.markCountAsRead(association, '/', 'mention');
  }

  async fetchMessages(newer: boolean, force = false): Promise<void> {
    const { api, station, graph } = this.props;

    if ( this.state.fetchPending && !force) {
     return new Promise((resolve, reject) => {});
    }

    this.setState({ fetchPending: true });

    const [,, ship, name] = station.split('/');
    const currSize = graph.size;
    if(newer && !this.loadedNewest) {
      const [index] = graph.peekLargest()!;
      await api.graph.getYoungerSiblings(ship,name, 20, `/${index.toString()}`)
      if(currSize === graph.size) {
        console.log('loaded all newest');
        this.loadedNewest = true;
      }
    } else if(!newer && !this.loadedOldest) {
      const [index] = graph.peekSmallest()!;
      await api.graph.getOlderSiblings(ship,name, 20, `/${index.toString()}`)
      this.calculateUnreadIndex();
      if(currSize === graph.size) {
        console.log('loaded all oldest');
        this.loadedOldest = true;
      }
    }
    this.setState({ fetchPending: false });

  }

  onScroll({ scrollTop, scrollHeight, windowHeight }) {
    if (!this.state.idle && scrollTop > IDLE_THRESHOLD) {
      this.setState({ idle: true });
    }

    this.dismissIfLineVisible();
  }

  dismissIfLineVisible() {
    if (this.props.unreadCount === 0) return;
    if (!this.unreadMarkerRef.current || !this.virtualList?.window) return;
    const parent = this.unreadMarkerRef.current.parentElement?.parentElement;
    if (!parent) return;
    const { scrollTop, scrollHeight, offsetHeight } = this.virtualList.window;
    if (
      (scrollHeight - parent.offsetTop > scrollTop)
      && (scrollHeight - parent.offsetTop < scrollTop + offsetHeight)
    ) {
      this.dismissUnread();
    }
  }

  render() {
    const {
      unreadCount,
      api,
      ship,
      station,
      association,
      group,
      contacts,
      graph,
      history,
      groups,
      associations
    } = this.props;

    const unreadMarkerRef = this.unreadMarkerRef;


    const messageProps = { association, group, contacts, unreadMarkerRef, history, api, groups, associations };

    const keys = graph.keys().reverse();
    const unreadIndex = graph.keys()[this.props.unreadCount];
    const unreadMsg = unreadIndex && graph.get(unreadIndex);

    return (
      <Col height='100%' overflow='hidden' position="relative">
        <UnreadNotice
          unreadCount={unreadCount}
          unreadMsg={unreadCount === 1 && unreadMsg && unreadMsg?.post.author === window.ship ? false : unreadMsg}
          dismissUnread={this.dismissUnread}
          onClick={this.scrollToUnread}
        />
        <VirtualScroller
          ref={list => {this.virtualList = list}}
          origin="bottom"
          style={{ height: '100%' }}
          onStartReached={() => {
            this.setState({ idle: false });
            this.dismissUnread();
          }}
          onScroll={this.onScroll.bind(this)}
          data={graph}
          size={graph.size}
          renderer={({ index, measure, scrollWindow }) => {
            const msg = graph.get(index)?.post;
            if (!msg) return null;
            if (!this.state.initialized) {
              return <MessagePlaceholder key={index.toString()} height="64px" index={index} />;
            }
            const isPending: boolean = 'pending' in msg && Boolean(msg.pending);
            const isLastMessage = index.eq(graph.peekLargest()?.[0] ?? bigInt.zero);
            const highlighted = bigInt(this.props.scrollTo || -1).eq(index);
            const graphIdx = keys.findIndex(idx => idx.eq(index));
            const prevIdx = keys[graphIdx+1];
            const nextIdx = keys[graphIdx-1];


            const isLastRead: boolean = this.state.unreadIndex.eq(index);
            const props = { measure, highlighted, scrollWindow, isPending, isLastRead, isLastMessage, msg, ...messageProps };
            return (
              <ChatMessage
                key={index.toString()}
                previousMsg={prevIdx && graph.get(prevIdx)?.post}
                nextMsg={nextIdx && graph.get(nextIdx)?.post}
                {...props}
              />
            );
          }}
          loadRows={(newer) => {
            this.fetchMessages(newer);
          }}
        />
      </Col>
    );
  }
}

