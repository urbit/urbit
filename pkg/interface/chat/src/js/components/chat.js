import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { Message } from '/components/lib/message';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';


export class ChatScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: '/' + props.match.params.station,
      numPeople: 0,
      numPages: 1,
      scrollLocked: false,
    };

    this.hasAskedForMessages = false;
    this.onScroll = this.onScroll.bind(this);

    this.updateReadInterval = setInterval(
      this.updateReadNumber.bind(this),
      1000
    );
  }

  componentDidMount() {
    this.updateNumPeople();
    this.updateReadNumber();
  }

  componentWillUnmount() {
    if (this.updateReadInterval) {
      clearInterval(this.updateReadInterval);
      this.updateReadInterval = null;
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps.match.params.station !== props.match.params.station) {
      this.hasAskedForMessages = false;

      clearInterval(this.updateReadInterval);

      this.setState({
        station: "/" + props.match.params.station,
        numPeople: 0,
        scrollLocked: false
      }, () => {
        this.updateNumPeople();
        this.scrollToBottom();
        this.updateReadInterval = setInterval(
          this.updateReadNumber.bind(this),
          1000
        );
        this.updateReadNumber();
      });
    } else if (Object.keys(props.inbox) > 0 && props.owner === '') {
      props.history.push('/~chat');
    }
  }

  updateReadNumber() {
    const { props, state } = this;

    let lastMsgNum = props.envelopes || [];
    lastMsgNum = lastMsgNum.length;
    let lastRead = props.read;
    if (lastMsgNum > lastRead && lastMsgNum > 0) {
      props.api.inbox.read(state.station, lastMsgNum);
    }
  }

  /*askForMessages() {
    const { props, state } = this;
    let messages = props.messages;
    
    if (state.numPages * 50 < props.messages.length - 200 ||
        this.hasAskedForMessages) {
      return;
    }

    if (messages.length > 0) {
      let end = messages[0].num;
      if (end > 0) {
        let start = ((end - 400) > 0) ? end - 400 : 0;

        this.hasAskedForMessages = true;

        console.log('fetching new messages');

        props.subscription.fetchMessages(state.station, start, end - 1);
      }
    }
  }*/

  scrollToBottom() {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView({ behavior: 'smooth' });
    }
  }

  onScroll(e) {
    if (navigator.userAgent.includes('Safari') &&
      navigator.userAgent.includes('Chrome')) {
      // Google Chrome
      if (e.target.scrollTop === 0) {
        this.setState({
          numPages: this.state.numPages + 1,
          scrollLocked: true
        }, () => {
          //this.askForMessages();
        });
      } else if (
          (e.target.scrollHeight - Math.round(e.target.scrollTop)) ===
          e.target.clientHeight
      ) {
        this.setState({
          numPages: 1,
          scrollLocked: false
        });
      }
    } else if (navigator.userAgent.includes('Safari')) {
      // Safari
      if (e.target.scrollTop === 0) {
        this.setState({
          numPages: 1,
          scrollLocked: false
        });
      } else if (
          (e.target.scrollHeight + Math.round(e.target.scrollTop)) ===
          e.target.clientHeight
      ) {
        this.setState({
          numPages: this.state.numPages + 1,
          scrollLocked: true
        }, () => {
          //this.askForMessages();
        });
      }
    } else {
      console.log('Your browser is not supported.');
    }
  }

  updateNumPeople() {
    return;
    let conf = this.props.configs[this.state.station] || {};
    let sis = _.get(conf, 'con.sis');
    let numPeople = !!sis ? sis.length : 0;
    if (numPeople !== this.state.numPeople) {
      this.setState({ numPeople });
    }
  }

  render() {
    const { props, state } = this;

    let messages = props.envelopes.slice(0);
    
    let lastMsgNum = (messages.length > 0) ?
      messages.length : 0;

    if (messages.length > 50 * state.numPages) {
      messages = messages
        .slice(messages.length - (50 * state.numPages), messages.length);
    }

    let pendingMessages = 
	    props.pendingMessages.has(state.station) 
      ? props.pendingMessages.get(state.station) : [];
	
    pendingMessages.map(function(value) {
      return value.pending = true;
    })
	
	  let reversedMessages = messages.concat(pendingMessages);
    reversedMessages = reversedMessages.reverse();

    reversedMessages = reversedMessages.map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      let aut = ['author'];
      let renderSigil =
        _.get(reversedMessages[i + 1], aut) !== _.get(msg, aut, msg.author);
      let paddingTop = renderSigil;
      let paddingBot =
        _.get(reversedMessages[i - 1], aut) !== _.get(msg, aut, msg.author);

      return (
        <Message
          key={msg.uid}
          msg={msg}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot}
          pending={!!msg.pending} />
      );
    });

    let group = Array.from(props.group.values());
    
    return (
      <div key={state.station}
        className="h-100 w-100 overflow-hidden flex flex-column">
        <div className='pl3 pt2 bb'>
          <h2>{state.station.substr(1)}</h2>
          <ChatTabBar {...props}
            station={state.station}
            numPeers={group.length}
            isOwner={props.owner === window.ship} />
        </div>
        <div
          className="overflow-y-scroll pt3 pb2 flex flex-column-reverse"
          style={{ height: 'calc(100% - 157px)', resize: 'vertical' }}
          onScroll={this.onScroll}>
          <div ref={ el => { this.scrollElement = el; }}></div>
          {reversedMessages}
        </div>
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={state.station}
          owner={props.owner}
          permissions={props.permissions}
          placeholder='Message...' />
      </div>
    )
  }
}
