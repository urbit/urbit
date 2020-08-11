import React, { Component, Fragment } from "react";

import { ChatMessage } from './chat-message';
import { ChatScrollContainer } from "./chat-scroll-container";
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

const MAX_BACKLOG_SIZE = 1000;
const DEFAULT_BACKLOG_SIZE = 200;
const PAGE_SIZE = 50;
const INITIAL_LOAD = 20;


export class ChatWindow extends Component {
  constructor(props) {
    super(props);
    this.state = {
      numPages: 1,
    };

    this.hasAskedForMessages = false;
    
    this.dismissUnread = this.dismissUnread.bind(this);
    this.scrollIsAtBottom = this.scrollIsAtBottom.bind(this);
    this.scrollIsAtTop = this.scrollIsAtTop.bind(this);

    this.scrollReference = React.createRef();
    this.unreadReference = React.createRef();
  }

  componentDidMount() {
    this.initialFetch();

    if (this.state.numPages === 1 && this.props.unreadCount < INITIAL_LOAD) {
      this.dismissUnread();
      this.scrollToBottom();
    }
  }

  initialFetch() {
    const { props } = this;
    if (props.messages.length > 0) {
      const unreadUnloaded = props.unreadCount - props.messages.length;

      if (unreadUnloaded <= MAX_BACKLOG_SIZE &&
          unreadUnloaded + INITIAL_LOAD > DEFAULT_BACKLOG_SIZE) {
        this.fetchBacklog(unreadUnloaded + INITIAL_LOAD);
      } else {
        this.fetchBacklog(DEFAULT_BACKLOG_SIZE);
      }
    } else {
      setTimeout(() => {
        this.initialFetch();
      }, 2000);
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (props.isChatMissing) {
      props.history.push("/~chat");
    } else if (props.messages.length >= prevProps.messages.length + 10) {
      this.hasAskedForMessages = false;
      let numPages = props.unreadCount > 0 ? 
        Math.ceil(props.unreadCount / PAGE_SIZE) : this.state.numPages;

      if (this.state.numPages === numPages) {
        if (props.unreadCount > 20) {
          this.scrollToUnread();    
        }
      } else {
        this.setState({ numPages }, () => {
          if (props.unreadCount > 20) {
            this.scrollToUnread();    
          }
        });
      }
    } else if (
      state.numPages === 1 &&
      this.props.unreadCount < INITIAL_LOAD &&
      this.props.unreadCount > 0
    ) {
      this.dismissUnread();
      this.scrollToBottom();
    }
  }

  scrollIsAtTop() {
    const { props, state } = this;
    this.setState({ numPages: state.numPages + 1 }, () => {
      if (state.numPages * PAGE_SIZE < props.length) {
        this.fetchBacklog(DEFAULT_BACKLOG_SIZE);
      }
    });
  }

  scrollIsAtBottom() {
    if (this.state.numPages !== 1) {
      this.setState({ numPages: 1 });
      this.dismissUnread();
    }
  }

  scrollToBottom() {
    if (this.scrollReference.current) {
      this.scrollReference.current.scrollToBottom();
    }
    if (this.state.numPages !== 1) {
      this.setState({ numPages: 1 });
    }
  }

  scrollToUnread() {
    if (this.scrollReference.current && this.unreadReference.current) {
      this.scrollReference.current.scrollToReference(this.unreadReference);
    }
  }

  dismissUnread() {
    this.props.api.chat.read(this.props.station);
  }

  fetchBacklog(size) {
    const { props } = this;

    if (
      props.messages.length >= props.length ||
      this.hasAskedForMessages ||
      props.length <= 0
    ) {
      return;
    }

    const start =
      props.length - props.messages[props.messages.length - 1].number;
    if (start > 0) {
      const end = start + size < props.length ? start + size : props.length;
      props.api.chat.fetchMessages(start + 1, end, props.station);
      this.hasAskedForMessages = true;
    }
  }

  render() {
    const { props, state } = this;
    const sliceLength = Math.min(
      state.numPages * PAGE_SIZE,
      props.messages.length + props.pendingMessages.length
    );
    const messages =
      props.pendingMessages
        .concat(props.messages)
        .slice(0, sliceLength);

    return (
      <Fragment>
        <UnreadNotice
          unreadCount={props.unreadCount}
          unreadMsg={props.unreadMsg}
          dismissUnread={this.dismissUnread} />
        <ChatScrollContainer
          ref={this.scrollReference}
          scrollIsAtBottom={this.scrollIsAtBottom}
          scrollIsAtTop={this.scrollIsAtTop}>
          <BacklogElement isChatLoading={props.isChatLoading} />
          <ResubscribeElement
            api={props.api}
            host={props.ship}
            station={props.station}
            isChatUnsynced={props.isChatUnsynced}
          />
          { messages.map((msg, i) => (
              <ChatMessage
                unreadRef={this.unreadReference}
                isLastUnread={
                  props.unreadCount > 0 && i === props.unreadCount - 1
                }
                msg={msg}
                previousMsg={messages[i - 1]}
                nextMsg={messages[i + 1]}
                association={props.association}
                group={props.group}
                contacts={props.contacts} />
            ))
          }
        </ChatScrollContainer>
      </Fragment>
    );
  }
}

