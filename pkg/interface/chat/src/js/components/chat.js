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
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
      numPeople: 0,
      numPages: 1,
      scrollLocked: false,
    };

    this.pendingQueue = props.pendingMessages;

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

    if (prevProps.match.params.ship !== props.match.params.ship ||
              prevProps.match.params.station !== props.match.params.station
    ) {
      console.log('switched circle');
      this.hasAskedForMessages = false;

      clearInterval(this.updateReadInterval);

      this.setState({
        station: props.match.params.ship + "/" + props.match.params.station,
        circle: props.match.params.station,
        host: props.match.params.ship,
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
    } else if (!(state.station in props.configs)) {
      props.history.push('/~chat');
    }
  }

  updateReadNumber() {
    const { props, state } = this;

    let internalCircle = 'hall-internal-' + state.circle;
    let internalStation = `~${window.ship}/${internalCircle}`;

    let internalConfig = props.configs[internalStation] || false;
    let regularConfig = props.configs[state.station] || false;

    let config = internalConfig || regularConfig;
    let messages = props.messages;

    let lastMsgNum = (messages.length > 0) ?
      ( messages[messages.length - 1].num + 1 ) : 0;

    if (config && config.red < lastMsgNum) {
      if (internalConfig) {
        props.api.read(internalCircle, lastMsgNum);
      } else {
        props.api.read(state.circle, lastMsgNum);
      }
    }
  }

  askForMessages() {
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
  }

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
          this.askForMessages();
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
          this.askForMessages();
        });
      }
    } else {
      console.log('Your browser is not supported.');
    }
  }

  updateNumPeople() {
    let conf = this.props.configs[this.state.station] || {};
    let sis = _.get(conf, 'con.sis');
    let numPeople = !!sis ? sis.length : 0;
    if (numPeople !== this.state.numPeople) {
      this.setState({ numPeople });
    }
  }

  render() {
    const { props, state } = this;

    let config = props.configs[state.station] || {};
    let messages = props.messages.slice(0);
    
    // Pending messages get pinned to the bottom of the messages queue.
    
    let pendingInRoom = 
    (this.pendingQueue.has(this.state.station)) 
    ? this.pendingQueue.get(this.state.station) : [];

    pendingInRoom.map(function(value) {
      return value.pending = true;
    })

    messages = messages.concat(pendingInRoom);

    let lastMsgNum = (messages.length > 0) ?
      messages[messages.length - 1].num : 0;

    if (messages.length > 50 * state.numPages) {
      messages = messages
        .slice(messages.length - (50 * state.numPages), messages.length);
    }

    let reversedMessages = messages.reverse();
    let chatMessages = reversedMessages.map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      let gamAut = ['gam', 'aut'];    

      // Local messages don't have a 'gam' prop, so look for the top level if it doesn't exist.
      let aut = msg.aut ? msg.aut : null;
      
      // No gamAut? Return top level author for the same sender check.
      let renderSigil =
        _.get(reversedMessages[i + 1], gamAut) !== _.get(msg, gamAut, aut);

      // More padding top if previous message is not by the same sender
      let paddingTop = renderSigil;
      // More padding bot if next message is not by the same sender
      let paddingBot =
        _.get(reversedMessages[i - 1], gamAut) !== _.get(msg, gamAut, aut);

      // Non-local ships don't have pending props.
      if (!msg.pending) {
        var pending = false;
      }

      // Non-local ships don't have pending props.
      if (!pending) {
        var pending = false;
      }

      return (
        <Message
          key={msg.gam ? msg.gam.uid : msg.uid}
          msg={msg.gam ? msg.gam : msg}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot} 
          pending={!!pending}/>
      );
    });

    let peers = props.peers[state.station] || [window.ship];

    return (
      <div key={state.station}
        className="h-100 w-100 overflow-hidden flex flex-column">
        <div className='pl3 pt2 bb'>
          <h2>{state.circle}</h2>
          <ChatTabBar {...props}
            station={state.station}
            numPeers={peers.length} />
        </div>
        <div
          className="overflow-y-scroll pt3 pb2 flex flex-column-reverse"
          style={{ height: 'calc(100% - 157px)', resize: 'vertical' }}
          onScroll={this.onScroll}>
          <div ref={ el => { this.scrollElement = el; }}></div>
          {chatMessages}
        </div>
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={state.station}
          circle={state.circle}
          security={config.con}
          placeholder='Message...' />
      </div>
    )
  }
}
