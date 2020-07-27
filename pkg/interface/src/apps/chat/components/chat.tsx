import React, { Component, Fragment } from "react";
import _ from "lodash";
import moment from "moment";

import { Link, RouteComponentProps } from "react-router-dom";

import { ResubscribeElement } from "./lib/resubscribe-element";
import { BacklogElement } from "./lib/backlog-element";
import { Message } from "./lib/message";
import { SidebarSwitcher } from "../../../components/SidebarSwitch";
import { ChatTabBar } from "./lib/chat-tabbar";
import { ChatInput } from "./lib/chat-input";
import { UnreadNotice } from "./lib/unread-notice";
import { deSig } from "../../../lib/util";
import { ChatHookUpdate } from "../../../types/chat-hook-update";
import ChatApi from "../../../api/chat";
import { Inbox, Envelope } from "../../../types/chat-update";
import { Contacts } from "../../../types/contact-update";
import { Path, Patp } from "../../../types/noun";
import GlobalApi from "../../../api/global";
import { Association } from "../../../types/metadata-update";
import {Group} from "../../../types/group-update";

function getNumPending(props: any) {
  const result = props.pendingMessages.has(props.station)
    ? props.pendingMessages.get(props.station).length
    : 0;
  return result;
}

const ACTIVITY_TIMEOUT = 60000; // a minute
const DEFAULT_BACKLOG_SIZE = 300;
const MAX_BACKLOG_SIZE = 1000;

function scrollIsAtTop(container) {
  if (
    (navigator.userAgent.includes("Safari") &&
      navigator.userAgent.includes("Chrome")) ||
    navigator.userAgent.includes("Firefox")
  ) {
    return container.scrollTop === 0;
  } else if (navigator.userAgent.includes("Safari")) {
    return (
      container.scrollHeight + Math.round(container.scrollTop) <=
      container.clientHeight + 10
    );
  } else {
    return false;
  }
}

function scrollIsAtBottom(container) {
  if (
    (navigator.userAgent.includes("Safari") &&
      navigator.userAgent.includes("Chrome")) ||
    navigator.userAgent.includes("Firefox")
  ) {
    return (
      container.scrollHeight - Math.round(container.scrollTop) <=
      container.clientHeight + 10
    );
  } else if (navigator.userAgent.includes("Safari")) {
    return container.scrollTop === 0;
  } else {
    return false;
  }
}

type IMessage = Envelope & { pending?: boolean };

type ChatScreenProps = RouteComponentProps<{
  ship: Patp;
  station: string;
}> & {
  chatSynced: ChatHookUpdate;
  station: any;
  association: Association;
  api: GlobalApi;
  read: number;
  length: number;
  inbox: Inbox;
  contacts: Contacts;
  group: Group;
  pendingMessages: Map<Path, Envelope[]>;
  s3: any;
  popout: boolean;
  sidebarShown: boolean;
  chatInitialized: boolean;
  envelopes: Envelope[];
};

interface ChatScreenState {
  numPages: number;
  scrollLocked: boolean;
  read: number;
  active: boolean;
  lastScrollHeight: number | null;
}

export class ChatScreen extends Component<ChatScreenProps, ChatScreenState> {
  hasAskedForMessages = false;
  lastNumPending = 0;

  scrollContainer: HTMLElement | null = null;

  unreadMarker = null;
  scrolledToMarker = false;

  activityTimeout: NodeJS.Timeout | null = null;

  scrollElement: HTMLElement | null = null;

  constructor(props) {
    super(props);

    this.state = {
      numPages: 1,
      scrollLocked: false,
      read: props.read,
      active: true,
      // only for FF
      lastScrollHeight: null,
    };

    this.onScroll = this.onScroll.bind(this);

    this.setUnreadMarker = this.setUnreadMarker.bind(this);

    this.handleActivity = this.handleActivity.bind(this);
    this.setInactive = this.setInactive.bind(this);

    moment.updateLocale("en", {
      calendar: {
        sameDay: "[Today]",
        nextDay: "[Tomorrow]",
        nextWeek: "dddd",
        lastDay: "[Yesterday]",
        lastWeek: "[Last] dddd",
        sameElse: "DD/MM/YYYY",
      },
    });
  }

  componentDidMount() {
    document.addEventListener("mousemove", this.handleActivity, false);
    document.addEventListener("mousedown", this.handleActivity, false);
    document.addEventListener("keypress", this.handleActivity, false);
    document.addEventListener("touchmove", this.handleActivity, false);
    this.activityTimeout = setTimeout(this.setInactive, ACTIVITY_TIMEOUT);
  }

  componentWillUnmount() {
    document.removeEventListener("mousemove", this.handleActivity, false);
    document.removeEventListener("mousedown", this.handleActivity, false);
    document.removeEventListener("keypress", this.handleActivity, false);
    document.removeEventListener("touchmove", this.handleActivity, false);
    if (this.activityTimeout) {
      clearTimeout(this.activityTimeout);
    }
  }

  handleActivity() {
    if (!this.state.active) {
      this.setState({ active: true });
    }

    if (this.activityTimeout) {
      clearTimeout(this.activityTimeout);
    }

    this.activityTimeout = setTimeout(this.setInactive, ACTIVITY_TIMEOUT);
  }

  setInactive() {
    this.activityTimeout = null;
    this.setState({ active: false, scrollLocked: true });
  }

  receivedNewChat() {
    const { props } = this;
    this.hasAskedForMessages = false;

    this.unreadMarker = null;
    this.scrolledToMarker = false;

    this.setState({ read: props.read });

    const unread = props.length - props.read;
    const unreadUnloaded = unread - props.envelopes.length;
    const excessUnread = unreadUnloaded > MAX_BACKLOG_SIZE;

    if (!excessUnread && unreadUnloaded + 20 > DEFAULT_BACKLOG_SIZE) {
      this.askForMessages(unreadUnloaded + 20);
    } else {
      this.askForMessages(DEFAULT_BACKLOG_SIZE);
    }

    if (excessUnread || props.read === props.length) {
      this.scrolledToMarker = true;
      this.setState(
        {
          scrollLocked: false,
        },
        () => {
          this.scrollToBottom();
        }
      );
    } else {
      this.setState({ scrollLocked: true, numPages: Math.ceil(unread / 100) });
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (
      prevProps.match.params.station !== props.match.params.station ||
      prevProps.match.params.ship !== props.match.params.ship
    ) {
      this.receivedNewChat();
    } else if (
      props.chatInitialized &&
      !(props.station in props.inbox) &&
      Boolean(props.chatSynced) &&
      !(props.station in props.chatSynced)
    ) {
      props.history.push("/~chat");
    } else if (props.envelopes.length >= prevProps.envelopes.length + 10) {
      this.hasAskedForMessages = false;
    } else if (
      props.length !== prevProps.length &&
      prevProps.length === prevState.read &&
      state.active
    ) {
      this.setState({ read: props.length });
      this.props.api.chat.read(this.props.station);
    }

    if (!prevProps.chatInitialized && props.chatInitialized) {
      this.receivedNewChat();
    }

    if (
      props.length !== prevProps.length ||
      props.envelopes.length !== prevProps.envelopes.length ||
      getNumPending(props) !== this.lastNumPending ||
      state.numPages !== prevState.numPages
    ) {
      this.scrollToBottom();
      if (navigator.userAgent.includes("Firefox")) {
        this.recalculateScrollTop();
      }

      this.lastNumPending = getNumPending(props);
    }
  }

  askForMessages(size) {
    const { props, state } = this;

    if (
      props.envelopes.length >= props.length ||
      this.hasAskedForMessages ||
      props.length <= 0
    ) {
      return;
    }

    const start =
      props.length - props.envelopes[props.envelopes.length - 1].number;
    if (start > 0) {
      const end = start + size < props.length ? start + size : props.length;
      this.hasAskedForMessages = true;
      props.api.chat.fetchMessages(start + 1, end, props.station);
    }
  }

  scrollToBottom() {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView();
    }
  }

  // Restore chat position on FF when new messages come in
  recalculateScrollTop() {
    const { lastScrollHeight } = this.state;
    if (!this.scrollContainer || !lastScrollHeight) {
      return;
    }

    const target = this.scrollContainer;
    const newScrollTop = this.scrollContainer.scrollHeight - lastScrollHeight;
    if (target.scrollTop !== 0 || newScrollTop === target.scrollTop) {
      return;
    }
    target.scrollTop = target.scrollHeight - lastScrollHeight;
  }

  onScroll(e) {
    if (scrollIsAtTop(e.target)) {
      // Save scroll position for FF
      if (navigator.userAgent.includes("Firefox")) {
        this.setState({
          lastScrollHeight: e.target.scrollHeight,
        });
      }
      this.setState(
        {
          numPages: this.state.numPages + 1,
          scrollLocked: true,
        },
        () => {
          this.askForMessages(DEFAULT_BACKLOG_SIZE);
        }
      );
    } else if (scrollIsAtBottom(e.target)) {
      this.dismissUnread();
      this.setState({
        numPages: 1,
        scrollLocked: false,
      });
    }
  }

  setUnreadMarker(ref) {
    if (ref && !this.scrolledToMarker) {
      this.setState({ scrollLocked: true }, () => {
        ref.scrollIntoView({ block: "center" });
        if (ref.offsetParent && scrollIsAtBottom(ref.offsetParent)) {
          this.dismissUnread();
          this.setState({
            numPages: 1,
            scrollLocked: false,
          });
        }
      });
      this.scrolledToMarker = true;
    }
    this.unreadMarker = ref;
  }

  dismissUnread() {
    this.props.api.chat.read(this.props.station);
  }

  chatWindow(unread) {
    // Replace with just the "not Firefox" implementation
    // when Firefox #1042151 is patched.

    const { props, state } = this;

    let messages: IMessage[] = props.envelopes.slice(0);
    const lastMsgNum = messages.length > 0 ? messages.length : 0;

    if (messages.length > 100 * state.numPages) {
      messages = messages.slice(0, 100 * state.numPages);
    }

    const pendingMessages: IMessage[] = (
      props.pendingMessages.get(props.station) || []
    ).map((value) => ({ ...value, pending: true }));

    if(unread !== 0) {
      unread += pendingMessages.length;
    }

    messages = pendingMessages.concat(messages);

    const messageElements = messages.map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      const aut = ["author"];
      const renderSigil =
        _.get(messages[i + 1], aut) !== _.get(msg, aut, msg.author);
      const paddingTop = renderSigil;
      const paddingBot =
        _.get(messages[i - 1], aut) !== _.get(msg, aut, msg.author);

      const when = ["when"];
      const dayBreak =
        moment(_.get(messages[i + 1], when)).format("YYYY.MM.DD") !==
        moment(_.get(messages[i], when)).format("YYYY.MM.DD");

      const messageElem = (
        <Message
          key={msg.uid}
          msg={msg}
          contacts={props.contacts}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot}
          pending={Boolean(msg.pending)}
          group={props.group}
          association={props.association}
        />
      );
      if (unread > 0 && i === unread - 1) {
        return (
          <Fragment key={msg.uid}>
            {messageElem}
            <div
              ref={this.setUnreadMarker}
              className="mv2 green2 flex items-center f9"
            >
              <hr className="dn-s ma0 w2 b--green2 bt-0" />
              <p className="mh4">New messages below</p>
              <hr className="ma0 flex-grow-1 b--green2 bt-0" />
              {dayBreak && (
                <p className="gray2 mh4">
                  {moment(_.get(messages[i], when)).calendar()}
                </p>
              )}
              <hr
                style={{ width: "calc(50% - 48px)" }}
                className="b--green2 ma0 bt-0"
              />
            </div>
          </Fragment>
        );
      } else if (dayBreak) {
        return (
          <Fragment key={msg.uid}>
            {messageElem}
            <div
              className="pv3 gray2 b--gray2 flex items-center justify-center f9 "
            >
              <p>{moment(_.get(messages[i], when)).calendar()}</p>
            </div>
          </Fragment>
        );
      } else {
        return messageElem;
      }
    });

    if (navigator.userAgent.includes("Firefox")) {
      return (
        <div
          className="relative overflow-y-scroll h-100"
          onScroll={this.onScroll}
          ref={(e) => {
            this.scrollContainer = e;
          }}
        >
          <div
            className="bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
            style={{ resize: "vertical" }}
          >
            <div
              ref={(el) => {
                this.scrollElement = el;
              }}
            ></div>
            {props.chatInitialized && !(props.station in props.inbox) && (
              <BacklogElement />
            )}
            {props.chatSynced &&
            !(props.station in props.chatSynced) &&
            messages.length > 0 ? (
              <ResubscribeElement
                api={props.api}
                host={props.match.params.ship}
                station={props.station}
              />
            ) : (
              <div />
            )}
            {messageElements}
          </div>
        </div>
      );
    } else {
      return (
        <div
          className="overflow-y-scroll bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse relative"
          style={{ height: "100%", resize: "vertical" }}
          onScroll={this.onScroll}
        >
          <div
            ref={(el) => {
              this.scrollElement = el;
            }}
          ></div>
          {props.chatInitialized && !(props.station in props.inbox) && (
            <BacklogElement />
          )}
          {props.chatSynced &&
          !(props.station in props.chatSynced) &&
          messages.length > 0 ? (
            <ResubscribeElement
              api={props.api}
              host={props.match.params.ship}
              station={props.station}
            />
          ) : (
            <div />
          )}
          {messageElements}
        </div>
      );
    }
  }

  render() {
    const { props, state } = this;

    const messages = props.envelopes.slice(0);

    const lastMsgNum = messages.length > 0 ? messages.length : 0;

    const group = Array.from(props.group.members);

    const isinPopout = props.popout ? "popout/" : "";

    const ownerContact =
      window.ship in props.contacts ? props.contacts[window.ship] : false;

    let title = props.station.substr(1);

    if (props.association && "metadata" in props.association) {
      title =
        props.association.metadata.title !== ""
          ? props.association.metadata.title
          : props.station.substr(1);
    }

    const unread = props.length - state.read;

    const unreadMsg = unread > 0 && messages[unread - 1];

    const showUnreadNotice =
      props.length !== props.read && props.read === state.read;

    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column relative"
      >
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}
        >
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>

        <div
          className={
            "pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative " +
            "overflow-x-auto overflow-y-hidden flex-shrink-0 "
          }
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={props.sidebarShown}
            popout={props.popout}
            api={props.api}
          />
          <Link
            to={"/~chat/" + isinPopout + "room" + props.station}
            className="pt2 white-d"
          >
            <h2
              className={
                "dib f9 fw4 lh-solid v-top " +
                (title === props.station.substr(1) ? "mono" : "")
              }
              style={{ width: "max-content" }}
            >
              {title}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            numPeers={group.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={props.popout}
            api={props.api}
          />
        </div>
        {!!unreadMsg && showUnreadNotice && (
          <UnreadNotice
            unread={unread}
            unreadMsg={unreadMsg}
            onRead={() => this.dismissUnread()}
          />
        )}
        {this.chatWindow(unread)}
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={props.station}
          owner={deSig(props.match.params.ship)}
          ownerContact={ownerContact}
          envelopes={props.envelopes}
          contacts={props.contacts}
          onEnter={() => this.setState({ scrollLocked: false })}
          s3={props.s3}
          placeholder="Message..."
        />
      </div>
    );
  }
}
