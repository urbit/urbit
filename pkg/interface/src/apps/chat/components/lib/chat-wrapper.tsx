import React, { Component, Fragment } from "react";

import { ChatScrollContainer } from "./chat-scroll-container";
import { ChatMessages } from "./chat-messages";
import { UnreadNotice } from "./unread-notice";
import { ResubscribeElement } from "./resubscribe-element";
import { BacklogElement } from "./backlog-element";

function getNumPending(props: any) {
  const result = props.pendingMessages.has(props.station)
    ? props.pendingMessages.get(props.station).length
    : 0;
  return result;
}

const MAX_BACKLOG_SIZE = 1000;
const DEFAULT_BACKLOG_SIZE = 300;


export class ChatWrapper extends Component {
  constructor(props) {
    super(props);

    this.state = {
      numPages: 1,
    };

    this.hasAskedForMessages = false;
    this.fetchPrevious = this.fetchPrevious.bind(this);
    this.dismissUnread = this.dismissUnread.bind(this);

    this.unreadReference = React.createRef();
    this.containerReference = React.createRef();

    this.shouldScrollToBottom = false;
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (
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
      this.dismissUnread();
    }

    let messagesChanged =
      props.length !== prevProps.length ||
      props.envelopes.length !== prevProps.envelopes.length ||
      getNumPending(props) !== this.lastNumPending ||
      state.numPages !== prevState.numPages;

    if (messagesChanged) {
      this.lastNumPending = getNumPending(props);
      this.scrollToBottom();
    }

  }

  scrollToBottom(scrollLocked) {
    if (this.containerReference.current) {
      this.containerReference.current.scrollToBottom(scrollLocked);
    }
    this.setState({ numPages: 1 });
  }

  receivedNewChat() {
    const { props } = this;
    this.hasAskedForMessages = false;

    const unread = props.length - props.read;
    const unreadUnloaded = unread - props.envelopes.length;
    const excessUnread = unreadUnloaded > MAX_BACKLOG_SIZE;
    
    console.log('new chat', unread, unreadUnloaded, excessUnread);

    if (!excessUnread && unreadUnloaded + 20 > DEFAULT_BACKLOG_SIZE) {
      this.fetchBacklog(unreadUnloaded + 20);
    } else {
      this.fetchBacklog(DEFAULT_BACKLOG_SIZE);
      this.dismissUnread();
    }

    if (excessUnread || props.read === props.length) {
      this.scrollToBottom(false);
      this.setState({ numPages: 1 });
    } else {
      this.scrollToBottom(true);
      this.setState({ numPages: Math.ceil(unread / 100) });
    }
  }

  dismissUnread() {
    this.props.api.chat.read(this.props.station);
  }

  fetchPrevious() {
    this.setState({ numPages: this.state.numPages + 1 }, () => {
      this.fetchBacklog();
    });
  }

  fetchBacklog() {
    const size = DEFAULT_BACKLOG_SIZE;
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

  render() {
    const { props, state } = this;

    const unread = props.length - props.read;
    console.log('unread in wrapper: ', unread);
    const unreadMsg = unread > 0 && props.envelopes[unread - 1];
    const showUnreadNotice = props.length !== props.read;
    
    return (
      <Fragment>
        {!!unreadMsg && showUnreadNotice && (
          <UnreadNotice
            unread={unread}
            unreadMsg={unreadMsg}
            onRead={() => this.dismissUnread()}
          />
        )}
        <ChatScrollContainer
          fetchPrevious={this.fetchPrevious}
          scrollReference={this.unreadReference}
          dismissUnread={this.dismissUnread}
          active={props.active}>
          { (props.chatInitialized && !(props.station in props.inbox)) ?
            <BacklogElement /> : null
          }
          { (props.chatSynced &&
             !(props.station in props.chatSynced) &&
             props.envelopes.length > 0
            ) ? 
              <ResubscribeElement
                api={props.api}
                host={props.match.params.ship}
                station={props.station}
              /> : null
          }
          <ChatMessages
            unread={unread}
            envelopes={props.envelopes}
            pendingMessages={props.pendingMessages}
            association={props.association}
            group={props.group}
            contacts={props.contacts}
            numPages={state.numPages}
            unreadRef={this.unreadReference} />
        </ChatScrollContainer>
      </Fragment>
    );
  }
}

