import React, { Component, Fragment } from "react";
import { Virtuoso as VirtualList, VirtuosoMethods } from 'react-virtuoso';

import { ChatMessage } from './chat-message';
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";
import { Envelope, IMessage } from "~/types/chat-update";
import { RouteComponentProps } from "react-router-dom";
import { Patp, Path } from "~/types/noun";
import { Contacts } from "~/types/contact-update";
import { Association } from "~/types/metadata-update";
import { Group } from "~/types/group-update";
import GlobalApi from "~/logic/api/global";
import _ from "lodash";
import { LocalUpdateRemoteContentPolicy } from "~/types";
import { ListRange } from "react-virtuoso/dist/engines/scrollSeekEngine";


const INITIAL_LOAD = 20;
const DEFAULT_BACKLOG_SIZE = 200;
const IDLE_THRESHOLD = 3;

const Placeholder = ({ height, index, className = '', style = {}, ...props }) => (
  <div className={`w-100 f7 pl3 pt4 pr3 cf flex lh-copy ${className}`} style={{ height, ...style }} {...props}>
      <div className="fl pr3 v-top bg-white bg-gray0-d">
        <span
          className="db bg-gray2 bg-white-d"
          style={{
            width: "24px",
            height: "24px",
            borderRadius: "50%",
            visibility: (index % 5 == 0) ? "initial" : "hidden",
          }}
        ></span>
      </div>
      <div className="fr clamp-message white-d" style={{ flexGrow: 1, marginTop: -8 }}>
        <div className="hide-child" style={{paddingTop: "6px", visibility: (index % 5 == 0) ? "initial" : "hidden" }}>
          <p className={`v-mid f9 gray2 dib mr3 c-default`}>
            <span className="mw5 db"><span className="bg-gray5 bg-gray1-d db w-100 h-100"></span></span>
          </p>
          <p className="v-mid mono f9 gray2 dib"><span className="bg-gray5 bg-gray1-d db w-100 h-100" style={{height: "1em", width: `${(index % 3 + 1) * 3}em`}}></span></p>
          <p className="v-mid mono f9 ml2 gray2 dib child dn-s"><span className="bg-gray5 bg-gray1-d db w-100 h-100"></span></p>
        </div>
        <span className="bg-gray5 bg-gray1-d db w-100 h-100 db" style={{height: `1em`, width: `${(index % 5) * 20}%`}}></span>
      </div>
    </div>
);


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
  range: ListRange;
  initialized: boolean;
}

export class ChatWindow extends Component<ChatWindowProps, ChatWindowState> {
  private unreadReference: React.RefObject<Component>;
  private virtualList: React.RefObject<VirtuosoMethods>;

  constructor(props) {
    super(props);

    this.state = {
      fetchPending: false,
      idle: (this.initialIndex() < props.mailboxSize - IDLE_THRESHOLD) ? true : false,
      range: { startIndex: 0, endIndex: 0},
      initialized: false
    };
    
    this.dismissUnread = this.dismissUnread.bind(this);
    this.initialIndex = this.initialIndex.bind(this);
    this.scrollToUnread = this.scrollToUnread.bind(this);

    this.unreadReference = React.createRef();
    this.virtualList = React.createRef();
  }

  componentDidMount() {
    this.initialFetch();
  }

  initialIndex() {
    const { mailboxSize, unreadCount } = this.props;
    return Math.min(Math.max(mailboxSize - 1 < INITIAL_LOAD
      ? 0
      : unreadCount // otherwise if there are unread messages
        ? mailboxSize - unreadCount - 1 // put the one right before at the top
        : mailboxSize - 1,
    0), mailboxSize);
  }

  initialFetch() {
    const { envelopes, mailboxSize, unreadCount } = this.props;
    if (envelopes.length > 0) {
      const start = Math.min(mailboxSize - unreadCount, mailboxSize - DEFAULT_BACKLOG_SIZE);
      this.fetchMessages(start, start + DEFAULT_BACKLOG_SIZE, true);
      const initialIndex = this.initialIndex();
      if (initialIndex < mailboxSize - IDLE_THRESHOLD) {
        this.setState({ idle: true });
      }
      if (unreadCount !== mailboxSize) {
        this.virtualList.current?.scrollToIndex({
          index: initialIndex,
          align: initialIndex <= 1 ? 'end' : 'start'
        });
        setTimeout(() => {
          this.setState({ initialized: true });
        }, 500);
      } else {
        this.setState({ initialized: true });
      }
      
    } else {
      setTimeout(() => {
        this.initialFetch();
      }, 2000);
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { isChatMissing, history, envelopes, mailboxSize, unreadCount } = this.props;
    let { idle } = this.state;

    if (isChatMissing) {
      history.push("/~chat");
    } else if (envelopes.length !== prevProps.envelopes.length && this.state.fetchPending) {
      this.setState({ fetchPending: false });
    }

    if (this.state.range.endIndex !== prevState.range.endIndex) {
      if (this.state.range.endIndex < mailboxSize - IDLE_THRESHOLD) {
        if (!idle) {
          idle = true;
        }
      } else if (idle && (unreadCount === 0 || this.state.range.endIndex === 0)) {
        idle = false;
      }
      this.setState({ idle });
    }

    if (!idle && idle !== prevState.idle) {
      setTimeout(() => {
        this.virtualList.current?.scrollToIndex(mailboxSize);
      }, 500)
    }

    if (!idle && prevProps.unreadCount !== unreadCount) {
      this.virtualList.current?.scrollToIndex(mailboxSize);
    }

    if  (!idle && envelopes.length !== prevProps.envelopes.length) {
      this.virtualList.current?.scrollToIndex(mailboxSize);
    }
  }

  scrollToUnread() {
    const { mailboxSize, unreadCount } = this.props;
    this.virtualList.current?.scrollToIndex({
      index: mailboxSize - unreadCount,
      align: 'center'
    });
  }

  dismissUnread() {
    this.props.api.chat.read(this.props.station);
  }

  fetchMessages(start, end, force = false) {
    start = Math.max(start, 0);
    end = Math.max(end, 0);
    const { api, mailboxSize, station } = this.props;

    if (
      (this.state.fetchPending ||
      mailboxSize <= 0)
      && !force
    ) {
      return;
    }
      
    api.chat
      .fetchMessages(Math.max(mailboxSize - end, 0), Math.min(mailboxSize - start, mailboxSize), station)
      .finally(() => {
        this.setState({ fetchPending: false });
      });

    this.setState({ fetchPending: true });
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
    } = this.props;
    
    const messages: Envelope[] = [];
    const debouncedFetch = _.debounce(this.fetchMessages, 500).bind(this);

    envelopes
      .forEach((message) => {
        messages[message.number] = message;
      });
    
    stationPendingMessages.sort((a, b) => a.when - b.when).forEach((message, index) => {
      messages[mailboxSize + index + 1] = message;
    });
    
    return (
      <Fragment>
        <UnreadNotice
          unreadCount={unreadCount}
          unreadMsg={this.state.idle ? unreadMsg : false}
          dismissUnread={this.dismissUnread}
          onClick={this.scrollToUnread}
        />
        <BacklogElement isChatLoading={isChatLoading} />
        <ResubscribeElement {...{ api, host: ship, station, isChatUnsynced}} />
        {messages.length ? <VirtualList
          ref={this.virtualList}
          style={{height: '100%', width: '100%', visibility: this.state.initialized ? 'initial' : 'hidden'}}
          totalCount={mailboxSize + stationPendingMessages.length}
          followOutput={!this.state.idle}
          endReached={this.dismissUnread}
          scrollSeek={{
            enter: velocity => Math.abs(velocity) > 2000,
            exit: velocity => Math.abs(velocity) < 200,
            change: (_velocity, _range) => {},
            placeholder: this.state.initialized ? Placeholder : () => <div></div>
          }}
          startReached={() => debouncedFetch(0, DEFAULT_BACKLOG_SIZE)}
          overscan={DEFAULT_BACKLOG_SIZE}
          rangeChanged={(range) => {
            this.setState({ range });
            debouncedFetch(range.startIndex - (DEFAULT_BACKLOG_SIZE / 2), range.endIndex + (DEFAULT_BACKLOG_SIZE / 2));
          }}
          item={(i) => {
            const number = i + 1;
            const msg = messages[number];
            
            if (!msg) {
              debouncedFetch(number - DEFAULT_BACKLOG_SIZE, number + DEFAULT_BACKLOG_SIZE);
              return <Placeholder index={number} height="0px" style={{overflow: 'hidden'}} />;
            }
            return  <ChatMessage
              key={number}
              unreadRef={this.unreadReference}
              isFirstUnread={
                unreadCount
                && mailboxSize - unreadCount === number
                && this.state.idle
              }
              msg={msg}
              previousMsg={messages[number + 1]}
              nextMsg={messages[number - 1]}
              association={association}
              group={group}
              contacts={contacts}
              hideAvatars={hideAvatars}
              hideNicknames={hideNicknames}
              remoteContentPolicy={remoteContentPolicy}
              className={number === mailboxSize + stationPendingMessages.length ? 'pb3' : ''}
            />
          }}
        /> : <div style={{height: '100%', width: '100%'}}></div>}
      </Fragment>
    );
  }
}

