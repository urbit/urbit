import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { Message } from '/components/lib/message';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';

import { prettyShip, getMessageContent } from '/lib/util';

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

    this.hasAskedForMessages = false;
    this.topMessage = {};
    this.buildMessage = this.buildMessage.bind(this);
    this.onScroll = this.onScroll.bind(this);

    this.scrollClosure = false;

    this.updateReadInterval = setInterval(
      this.updateReadNumber.bind(this),
      1000
    );

  }

  componentDidMount() {
    this.updateNumPeople();
    if (this.scrollElement) {
      this.scrollElement.scrollIntoView(false, { block: 'end' });
    }
  }

  componentWillUnMount() {
    if (this.updateReadInterval) {
      clearInterval(this.updateReadInterval);
      this.updateReadInterval = null;
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props } = this;

    if (prevProps.match.params.ship !== props.match.params.ship ||
              prevProps.match.params.station !== props.match.params.station
    ) {
      console.log('switched circle');
      this.hasAskedForMessages = false;
      this.scrollClosure = false;
      this.topMessage = {};
 
      this.setState({
        station: props.match.params.ship + "/" + props.match.params.station,
        circle: props.match.params.station,
        host: props.match.params.ship,
        numPeople: 0,
        scrollLocked: false
      }, () => {
        this.updateNumPeople();
        this.scrollToBottom();
      });
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
    if (e.target.scrollTop === 0
      && this.state.numPages * 50 <= this.props.messages.length) {
      this.setState({
        numPages: this.state.numPages + 1,
        scrollLocked: true
      }, () => {
        if (this.topMessage && this.topMessage[1]) {
          this.askForMessages();
          this.topMessage[1].scrollIntoView(true);
        } 
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
  }

  updateNumPeople() {
    let conf = this.props.configs[this.state.station] || {};
    let sis = _.get(conf, 'con.sis');
    let numPeople = !!sis ? sis.length : 0;
    if (numPeople !== this.state.numPeople) {
      this.setState({ numPeople });
    }
  }

  buildMessage(msg, index) {
    let details = msg.printship ? null : getMessageContent(msg.gam);

    if (msg.printship) {
      return (
        <a 
          className="vanilla hoverline text-600 text-mono" 
          href={prettyShip(msg.gam.aut)[1]}>
          {prettyShip(`~${msg.gam.aut}`)[0]}
        </a>
      );
    }

    if (index % 50 === 0) {
      let pageNum = index / 50;
      return (
        <div
          key={msg.gam.uid + "key" + Math.random() + "key" + msg.num}
          ref={ el => { this.topMessage[pageNum] = el; }}>
          <Message msg={msg.gam} details={details} />
        </div>
      );
    } else {
      return (
        <Message
          key={msg.gam.uid + Math.random()} 
          msg={msg.gam} 
          details={details} />
      );
    }
  }

  render() {
    const { props, state } = this;
    

    let messages = props.messages.slice(0);

    let lastMsgNum = (messages.length > 0) ?
      messages[messages.length - 1].num : 0;

    if (messages.length > 50 * state.numPages) {
      messages = messages
        .slice(messages.length - (50 * state.numPages), messages.length);
    }

    let chatMessages = messages.map(this.buildMessage);
    let peers = props.peers[state.station] || [window.ship];

    return (
      <div key={state.station} 
        className="h-100 w-100 overflow-hidden flex flex-column">
        <div className='pl2 pt2 bb mb3'>
          <h2>{state.circle}</h2>
          <ChatTabBar {...props}
            station={state.station}
            numPeers={peers.length} />
        </div>
        <div
          className="overflow-y-scroll"
          style={{ flexGrow: 1 }}
          onScroll={this.onScroll}>
          {chatMessages}
          <div ref={ el => { this.scrollElement = el; }}></div>
        </div>
        <ChatInput 
          api={props.api}
          numMsgs={lastMsgNum}
          station={state.station}
          circle={state.circle}
          placeholder='Message...' />
      </div>
    )
  }
}

