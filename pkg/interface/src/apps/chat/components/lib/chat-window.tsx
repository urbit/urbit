import React, { Component, Fragment } from "react";

import { scrollIsAtTop, scrollIsAtBottom } from "../../../../lib/util";

import { ChatMessages } from './chat-messages';
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

const DEFAULT_BACKLOG_SIZE = 300;
const MAX_BACKLOG_SIZE = 1000;

function getNumPending(props: any) {
  const result = props.pendingMessages.has(props.station)
    ? props.pendingMessages.get(props.station).length
    : 0;
  return result;
}


export class ChatWindow extends Component {
  constructor(props) {
    super(props);

    this.state = {
      read: props.read,
      numPages: 1,
      scrollLocked: false,
      // only for FF
      lastScrollHeight: null
    };

    this.scrollContainer = null;
    this.unreadMarker = null;
    this.hasAskedForMessages = false;
    this.scrolledToMarker = false;

    this.onScroll = this.onScroll.bind(this);
    this.setUnreadMarker = this.setUnreadMarker.bind(this);
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
      props.active
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
      //  TODO: figure out what to do
      this.scrollToBottom();
      if (navigator.userAgent.includes("Firefox")) {
        this.recalculateScrollTop();
      }

      this.lastNumPending = getNumPending(props);
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

  scrollToBottom() {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView();
    }
  }

  firefoxScrollContainer(children) {
    return (
      <div
        className="relative overflow-y-scroll h-100"
        onScroll={this.onScroll}
        ref={(e) => {
          this.scrollContainer = e;
        }}>
        <div
          className="bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
          style={{ resize: "vertical" }}>
          <div ref={(el) => { this.scrollElement = el; }}></div>
          {children}
        </div>
      </div>
    );
  }

  normalScrollContainer(children) {
    return (
      <div
        className={
          "overflow-y-scroll bg-white bg-gray0-d pt3 pb2 flex " +
          "flex-column-reverse relative"
        }
        style={{ height: "100%", resize: "vertical" }}
        onScroll={this.onScroll}>
        <div
          ref={(el) => {
            this.scrollElement = el;
          }}
        ></div>
        {children}
      </div>
    );
  }

  childrenElements() {
    const { props, state } = this;
    const unread = props.length - state.read;

    return (
      <Fragment>
        {props.chatInitialized && !(props.station in props.inbox) && (
          <BacklogElement />
        )}
        {props.chatSynced &&
        !(props.station in props.chatSynced) && props.envelopes.length > 0 ? (
          <ResubscribeElement
            api={props.api}
            host={props.match.params.ship}
            station={props.station}
          />
        ) : (
          <div />
        )}
        <ChatMessages
          unread={unread}
          envelopes={props.envelopes}
          pendingMessages={props.pendingMessages}
          contacts={props.contacts}
          numPages={state.numPages} />
      </Fragment>
    );
  }

  render() {
    const { props, state } = this;

    const unread = props.length - state.read;
    const unreadMsg = unread > 0 && props.envelopes[unread - 1];
    const showUnreadNotice =
      props.length !== props.read && props.read === state.read;
    
    const childrenElements = this.childrenElements();

    // Replace with just the "not Firefox" implementation
    // when Firefox #1042151 is patched.
    return (
      <Fragment>
        {!!unreadMsg && showUnreadNotice && (
          <UnreadNotice
            unread={unread}
            unreadMsg={unreadMsg}
            onRead={() => this.dismissUnread()}
          />
        )}
        { navigator.userAgent.includes("Firefox") ?
            this.firefoxScrollContainer(childrenElements) :
            this.normalScrollContainer(childrenElements)
        }
      </Fragment>
    );
  }
}

