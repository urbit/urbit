import React, { Component } from "react";
import { RouteComponentProps } from "react-router-dom";
import _ from "lodash";

import GlobalApi from "~/logic/api/global";
import { Patp, Path } from "~/types/noun";
import { Contacts } from "~/types/contact-update";
import { Association } from "~/types/metadata-update";
import { Group } from "~/types/group-update";
import { Envelope, IMessage } from "~/types/chat-update";
import { LocalUpdateRemoteContentPolicy } from "~/types";

import VirtualScroller from "~/views/components/VirtualScroller";

import ChatMessage, { MessagePlaceholder } from './ChatMessage';
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

const INITIAL_LOAD = 20;
const DEFAULT_BACKLOG_SIZE = 100;
const IDLE_THRESHOLD = 64;
const MAX_BACKLOG_SIZE = 1000;

type ChatWindowProps = RouteComponentProps<{
  ship: Patp;
  station: string;
}> & {
  unreadCount: number;
  envelopes: Envelope[];
  isChatMissing: boolean;
  isChatLoading: boolean;
  isChatUnsynced: boolean;
  unreadMsg: Envelope | false;
  stationPendingMessages: IMessage[];
  mailboxSize: number;
  contacts: Contacts;
  association: Association;
  group: Group;
  ship: Patp;
  station: any;
  allStations: any;
  api: GlobalApi;
  hideNicknames: boolean;
  hideAvatars: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

interface ChatWindowState {
  fetchPending: boolean;
  idle: boolean;
  initialized: boolean;
  lastRead: number;
}

export default class ChatWindow extends Component<ChatWindowProps, ChatWindowState> {
  private virtualList: VirtualScroller | null;
  private unreadMarkerRef: React.RefObject<HTMLDivElement>;
  
  INITIALIZATION_MAX_TIME = 1500;

  constructor(props) {
    super(props);

    this.state = {
      fetchPending: false,
      idle: true,
      initialized: false,
      lastRead: props.unreadCount ? props.mailboxSize - props.unreadCount : Infinity,
    };
    
    this.dismissUnread = this.dismissUnread.bind(this);
    this.scrollToUnread = this.scrollToUnread.bind(this);
    this.handleWindowBlur = this.handleWindowBlur.bind(this);
    this.handleWindowFocus = this.handleWindowFocus.bind(this);
    this.stayLockedIfActive = this.stayLockedIfActive.bind(this);
    this.dismissIfLineVisible = this.dismissIfLineVisible.bind(this);
    
    this.virtualList = null;
    this.unreadMarkerRef = React.createRef();
  }

  componentDidMount() {
    window.addEventListener('blur', this.handleWindowBlur);
    window.addEventListener('focus', this.handleWindowFocus);
    this.initialFetch();
    setTimeout(() => {
      this.setState({ initialized: true });
    }, this.INITIALIZATION_MAX_TIME);
  }
  
  componentWillUnmount() {
    window.removeEventListener('blur', this.handleWindowBlur);
    window.removeEventListener('focus', this.handleWindowFocus);
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

  initialFetch() {
    const { envelopes, mailboxSize, unreadCount } = this.props;
    if (envelopes.length > 0) {
      const start = Math.min(mailboxSize - unreadCount, mailboxSize - DEFAULT_BACKLOG_SIZE);
      this.stayLockedIfActive();
      this.fetchMessages(start, start + DEFAULT_BACKLOG_SIZE, true).then(() => {
        if (!this.virtualList) return;
        this.setState({ idle: false });
        this.setState({ initialized: true });
        this.dismissIfLineVisible();
      });
    } else {
      setTimeout(() => {
        this.initialFetch();
      }, 2000);
    }
  }

  componentDidUpdate(prevProps: ChatWindowProps, prevState) {
    const { isChatMissing, history, envelopes, mailboxSize, stationPendingMessages, unreadCount, station } = this.props;

    if (isChatMissing) {
      history.push("/~404");
    } else if (envelopes.length !== prevProps.envelopes.length && this.state.fetchPending) {
      this.setState({ fetchPending: false });
    }

    if ((mailboxSize !== prevProps.mailboxSize) || (envelopes.length !== prevProps.envelopes.length)) {
      this.virtualList?.calculateVisibleItems();
      this.stayLockedIfActive();
    }

    if (unreadCount > prevProps.unreadCount && this.state.idle) {
      this.setState({
        lastRead: unreadCount ? mailboxSize - unreadCount : Infinity,
      });
    }

    if (stationPendingMessages.length !== prevProps.stationPendingMessages.length) {
      this.virtualList?.calculateVisibleItems();
    }

    if (!this.state.fetchPending && prevState.fetchPending) {
      this.virtualList?.calculateVisibleItems();
    }

    if (station !== prevProps.station) {
      this.virtualList?.resetScroll();
      this.scrollToUnread();
      this.setState({
        lastRead: unreadCount ? mailboxSize - unreadCount : Infinity,
      });
    }
  }

  stayLockedIfActive() {
    if (this.virtualList && !this.state.idle) {
      this.virtualList.resetScroll();
      this.dismissUnread();
    }
  }

  scrollToUnread() {
    const { mailboxSize, unreadCount } = this.props;
    this.virtualList?.scrollToData(mailboxSize - unreadCount);
  }

  dismissUnread() {
    if (this.state.fetchPending) return;
    if (this.props.unreadCount === 0) return;
    this.props.api.chat.read(this.props.station);
  }

  fetchMessages(start, end, force = false): Promise<void> {
    start = Math.max(start, 0);
    end = Math.max(end, 0);
    const { api, mailboxSize, station } = this.props;

    if (
      (this.state.fetchPending ||
      mailboxSize <= 0)
      && !force
    ) {
      return new Promise((resolve, reject) => {});
    }

    this.setState({ fetchPending: true });
    
    start = Math.min(mailboxSize - start, mailboxSize);
    end = Math.max(mailboxSize - end, 0, start - MAX_BACKLOG_SIZE);
    
    return api.chat
      .fetchMessages(end, start, station)
      .finally(() => {
        this.setState({ fetchPending: false });
      });
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
      envelopes,
      stationPendingMessages,
      unreadCount,
      unreadMsg,
      isChatLoading,
      isChatUnsynced,
      api,
      ship,
      station,
      association,
      group,
      contacts,
      mailboxSize,
      hideAvatars,
      hideNicknames,
      remoteContentPolicy,
      allStations,
      history
    } = this.props;

    const unreadMarkerRef = this.unreadMarkerRef;

    const messages = new Map();
    let lastMessage = 0;
    
    [...envelopes]
      .sort((a, b) => a.number - b.number)
      .forEach(message => {
        messages.set(message.number, message);
        lastMessage = message.number;
      });

    stationPendingMessages
      .sort((a, b) => a.when - b.when)
      .forEach((message, index) => {
        index = index + 1; // To 1-index it
        messages.set(mailboxSize + index, message);
        lastMessage = mailboxSize + index;
      });

    const messageProps = { association, group, contacts, hideAvatars, hideNicknames, remoteContentPolicy, unreadMarkerRef, allStations, history, api };
    
    return (
      <>
        <UnreadNotice
          unreadCount={unreadCount}
          unreadMsg={unreadCount === 1 && unreadMsg && unreadMsg.author === window.ship ? false : unreadMsg}
          dismissUnread={this.dismissUnread}
          onClick={this.scrollToUnread}
        />
        <BacklogElement isChatLoading={isChatLoading} />
        <ResubscribeElement {...{ api, host: ship, station, isChatUnsynced}} />
        <VirtualScroller
          ref={list => {this.virtualList = list}}
          origin="bottom"
          style={{ height: '100%' }}
          onStartReached={() => {
            this.setState({ idle: false });
            this.dismissUnread();
          }}
          onScroll={this.onScroll.bind(this)}
          data={messages}
          size={mailboxSize + stationPendingMessages.length}
          renderer={({ index, measure, scrollWindow }) => {
            const msg: Envelope | IMessage = messages.get(index);
            if (!msg) return null;
            if (!this.state.initialized) {
              return <MessagePlaceholder key={index} height="64px" index={index} />;
            }
            const isPending: boolean = 'pending' in msg && Boolean(msg.pending);
            const isLastMessage: boolean = Boolean(index === lastMessage)
            const isLastRead: boolean = Boolean(!isLastMessage && index === this.state.lastRead);
            const props = { measure, scrollWindow, isPending, isLastRead, isLastMessage, msg, ...messageProps };
            return (
              <ChatMessage
                key={index}
                previousMsg={messages.get(index + 1)}
                nextMsg={messages.get(index - 1)}
                {...props}
              />
            );
          }}
          loadRows={(start, end) => {
            this.fetchMessages(start, end);
          }}
        />
      </>
    );
  }
}

