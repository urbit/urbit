import React, { Component, Fragment } from "react";

import { ChatMessage } from './chat-message';
import { ChatScrollContainer } from "./chat-scroll-container";
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

const MAX_BACKLOG_SIZE = 1000;
const DEFAULT_BACKLOG_SIZE = 200;


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
  }

  componentDidMount() {
    this.initialFetch();
  }

  initialFetch() {
    const { props } = this;
    if (props.messages.length <= 0) {
      const unreadUnloaded = props.unreadCount - props.messages.length;

      if (unreadUnloaded <= MAX_BACKLOG_SIZE &&
          unreadUnloaded + 20 > DEFAULT_BACKLOG_SIZE) {
        this.fetchBacklog(unreadUnloaded + 20);
      } else {
        this.fetchBacklog(DEFAULT_BACKLOG_SIZE);
      }
    } else {
      setTimeout(() => {
        this.initialFetch();
      }, 5000);
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (props.isChatMissing) {
      props.history.push("/~chat");
    } else if (props.messages.length >= prevProps.messages.length + 10) {
      this.hasAskedForMessages = false;
      if (props.unreadCount === 0) {
        return;
      }
      this.setState({ numPages: Math.ceil(props.unreadCount / 100) });
    } else if (state.numPages === 1 && props.unreadCount !== 0) {
      this.dismissUnread();
      this.scrollToBottom();
    }
  }

  scrollIsAtTop() {
    const { props, state } = this;
    this.setState({ numPages: state.numPages + 1 }, () => {
      if (state.numPages * 100 < props.length) {
        this.fetchBacklog(DEFAULT_BACKLOG_SIZE);
      }
    });
  }

  scrollIsAtBottom() {
    if (this.state.numPages !== 1) {
      this.setState({ numPages: 1 });
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
    const sliceLength = (
      state.numPages * 100 <
      props.messages.length + props.pendingMessages.length
    ) ? state.numPages * 100 :
        props.messages.length + props.pendingMessages.length;

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
                unread={props.unreadCount}
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

