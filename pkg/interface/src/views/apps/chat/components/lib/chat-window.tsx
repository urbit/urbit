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

import ChatMessage, { MessagePlaceholder } from './chat-message';
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

const INITIAL_LOAD = 20;
const DEFAULT_BACKLOG_SIZE = 100;
const IDLE_THRESHOLD = 64;

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
  api: GlobalApi;
  hideNicknames: boolean;
  hideAvatars: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

interface ChatWindowState {
  fetchPending: boolean;
  idle: boolean;
  initialized: boolean;
  lastMessageNumber: number;
  messagesToRender: Map<number, Envelope | IMessage>;
}

export class ChatWindow extends Component<ChatWindowProps, ChatWindowState> {
  private unreadReference: React.RefObject<HTMLDivElement>;
  private virtualList: VirtualScroller | null;
  
  INITIALIZATION_MAX_TIME = 1500;

  constructor(props) {
    super(props);

    this.state = {
      fetchPending: false,
      idle: true,
      initialized: false,
      lastMessageNumber: 0,
      messagesToRender: new Map(),
    };
    
    this.dismissUnread = this.dismissUnread.bind(this);
    this.initialIndex = this.initialIndex.bind(this);
    this.scrollToUnread = this.scrollToUnread.bind(this);
    this.handleWindowBlur = this.handleWindowBlur.bind(this);
    this.handleWindowFocus = this.handleWindowFocus.bind(this);
    this.stayLockedIfActive = this.stayLockedIfActive.bind(this);
    this.assembleMessages = this.assembleMessages.bind(this);
    
    this.virtualList = null;
    this.unreadReference = React.createRef();
  }

  componentDidMount() {
    window.addEventListener('blur', this.handleWindowBlur);
    window.addEventListener('focus', this.handleWindowFocus);
    this.initialFetch();
    this.assembleMessages();
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
  }

  initialIndex() {
    const { unreadCount } = this.props;
    const { lastMessageNumber } = this.state;
    return Math.min(Math.max(lastMessageNumber - 1 < INITIAL_LOAD
      ? 0
      : unreadCount // otherwise if there are unread messages
        ? lastMessageNumber - unreadCount // put the one right before at the top
        : lastMessageNumber,
    0), lastMessageNumber);
  }

  initialFetch() {
    const { envelopes, mailboxSize, unreadCount } = this.props;
    if (envelopes.length > 0) {
      const start = Math.min(mailboxSize - unreadCount, mailboxSize - DEFAULT_BACKLOG_SIZE);
      this.stayLockedIfActive();
      this.fetchMessages(start, start + DEFAULT_BACKLOG_SIZE, true).then(() => {
        if (!this.virtualList) return;
        const initialIndex = this.initialIndex();
        this.virtualList.scrollToData(initialIndex).then(() => {
          if (
            initialIndex === mailboxSize
            || (this.virtualList && this.virtualList.window && this.virtualList.window.scrollTop === 0)
          ) {
            this.setState({ idle: false });
            this.dismissUnread();
          }
          this.setState({ initialized: true });
        });
      });
    } else {
      setTimeout(() => {
        this.initialFetch();
      }, 2000);
    }
  }

  componentDidUpdate(prevProps: ChatWindowProps, prevState) {
    const { isChatMissing, history, envelopes, mailboxSize, stationPendingMessages } = this.props;

    if (isChatMissing) {
      history.push("/~chat");
    } else if (envelopes.length !== prevProps.envelopes.length && this.state.fetchPending) {
      this.setState({ fetchPending: false });
    }

    if ((mailboxSize !== prevProps.mailboxSize) || (envelopes.length !== prevProps.envelopes.length)) {
      this.virtualList?.calculateVisibleItems();
      this.stayLockedIfActive();
      this.assembleMessages();
    }

    if (stationPendingMessages.length !== prevProps.stationPendingMessages.length) {
      this.virtualList?.calculateVisibleItems();
      this.virtualList?.scrollToData(mailboxSize);
      this.assembleMessages();
    }

    if (!this.state.fetchPending && prevState.fetchPending) {
      this.virtualList?.calculateVisibleItems();
    }
  }

  assembleMessages() {
    const { envelopes, stationPendingMessages } = this.props;
    const messages: Map<number, Envelope | IMessage> = new Map();
    let lastMessageNumber = 0;

    [...envelopes]
      .sort((a, b) => a.when - b.when)
      .forEach((message) => {
        messages.set(message.number, message);
        if (message.number > lastMessageNumber) {
          lastMessageNumber = message.number;
        }
      });

    if (lastMessageNumber !== this.state.lastMessageNumber) {
      this.setState({ lastMessageNumber });
    }
    
    stationPendingMessages.sort((a, b) => a.when - b.when).forEach((message, index) => {
      messages.set(lastMessageNumber + index + 1, message);
    });

    this.setState({ messagesToRender: messages });
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
    
    return api.chat
      .fetchMessages(Math.max(mailboxSize - end, 0), Math.min(mailboxSize - start, mailboxSize), station)
      .finally(() => {
        this.setState({ fetchPending: false });
      });
  }
  
  render() {
    const {
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
    } = this.props;
    
    const messages = this.state.messagesToRender;
    
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
          onScroll={({ scrollTop }) => {
            if (!this.state.idle && scrollTop > IDLE_THRESHOLD) {
              this.setState({ idle: true });
            }
          }}
          data={messages}
          size={mailboxSize + stationPendingMessages.length}
          renderer={({ index, measure, scrollWindow }) => {
            const msg = messages.get(index);
            if (!msg) return null;
            if (!this.state.initialized) {
              return <MessagePlaceholder key={index} height="64px" index={index} />;
            }
            return (
              <ChatMessage
                measure={measure}
                scrollWindow={scrollWindow}
                key={index}
                unreadRef={this.unreadReference}
                isPending={msg && 'pending' in msg && Boolean(msg.pending)}
                isFirstUnread={
                  Boolean(unreadCount
                  && this.state.lastMessageNumber - unreadCount === index
                  && !(unreadCount === 1 && msg.author === window.ship))
                }
                msg={msg}
                previousMsg={messages.get(index + 1)}
                nextMsg={messages.get(index - 1)}
                association={association}
                group={group}
                contacts={contacts}
                hideAvatars={hideAvatars}
                hideNicknames={hideNicknames}
                remoteContentPolicy={remoteContentPolicy}
                className={index === this.state.lastMessageNumber + stationPendingMessages.length ? 'pb3' : ''}
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

