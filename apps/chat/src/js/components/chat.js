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
      scrollLocked: false
    };

    this.topMessage = {};
    this.buildMessage = this.buildMessage.bind(this);
    this.onScroll = this.onScroll.bind(this);
  }

  componentDidMount() {
    this.updateNumPeople();
    this.scrollElement.scrollIntoView(false);
  }

  scrollToBottom() {
    if (!this.state.scrollLocked) {
      console.log('scroll to bottom');
      this.scrollElement.scrollIntoView({ behavior: 'smooth' });
    }
  }

  onScroll(e) {
    if (e.target.scrollTop === 0) {
      let topMessage = this.topMessage;

      this.setState({
        numPages: this.state.numPages + 1,
        scrollLocked: true
      }, () => {
        this.topMessage[1].scrollIntoView(true);
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

  componentDidUpdate(prevProps, prevState) {
    const { props } = this;

    if (prevProps !== props) {
      this.setState({
        station: props.match.params.ship + "/" + props.match.params.station,
        circle: props.match.params.station,
        host: props.match.params.ship,
        numPeople: 0
      });
    }

    this.updateReadNumber();
    this.updateNumPeople();
    this.updateNumMessagesLoaded(prevProps, prevState);
    this.scrollToBottom();
  }

  updateReadNumber() {
    const { props, state } = this;
    
    let messages = props.messages[state.station] || [];
    let config = props.configs[state.station] || false;

    let lastMsgNum = (messages.length > 0) ?
      messages[messages.length - 1].num : 0;

    if (config && config.red > lastMsgNum) {
      props.api.read(circle, lastMsgNum);
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

  updateNumMessagesLoaded(prevProps, prevState) {
    let station = prevProps.messages[this.state.station] || [];
    let numMessages = station.length;

    if (numMessages > prevState.numMessages) {
      this.setState({
        numMessages: numMessages
      });
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
        <div ref={ el => { this.topMessage[pageNum] = el; }}>
          <Message 
            key={msg.gam.uid} msg={msg.gam} details={details} />
        </div>
      );
    } else {
      return (
        <Message key={msg.gam.uid} msg={msg.gam} details={details} />
      );
    }
  }

  render() {
    const { props, state } = this;
    let messages = this.props.messages[this.state.station] || [];
    if (messages.length > 50 * state.numPages) {
      messages =
        messages.slice(messages.length - (50 * state.numPages), messages.length);
    }
    let chatMessages = messages.map(this.buildMessage);

    return (
      <div className="h-100 w-100 overflow-hidden flex flex-column">
        <div className='pl2 pt2 bb mb3'>
          <h2>{this.state.circle}</h2>
          <ChatTabBar {...this.props} station={this.state.station} />
        </div>
        <div
          className="overflow-y-scroll"
          style={{ flexGrow: 1 }}
          onScroll={this.onScroll}>
          {chatMessages}
          <div ref={ el => { this.scrollElement = el; }}></div>
        </div>
        <ChatInput 
          api={this.props.api}
          configs={this.props.configs}
          station={this.state.station}
          circle={this.state.circle}
          placeholder='Message...' />
      </div>
    )
  }
}

