import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { Message } from '/components/lib/message';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';

import { prettyShip, getMessageContent, isDMStation } from '/lib/util';

export class ChatScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
      numPeople: 0
    };

    this.buildMessage = this.buildMessage.bind(this);
  }

  componentDidMount() {
    this.updateNumPeople();
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

  buildMessage(msg) {
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
    return (
      <Message key={msg.gam.uid} msg={msg.gam} details={details} />
    );
  }

  render() {
    let messages = this.props.messages[this.state.station] || [];
    let chatMessages = messages.map(this.buildMessage);

    return (
      <div className="h-100 w-100 overflow-hidden flex flex-column">
        <div className='pl2 pt2 bb mb3'>
          <h2>{this.state.circle}</h2>
          <ChatTabBar {...this.props} station={this.state.station} />
        </div>
        <div className="overflow-y-scroll" style={{ flexGrow: 1 }}>
          {chatMessages}
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

