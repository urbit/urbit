import React, { Component } from 'react';
import { Scrollbars } from 'react-custom-scrollbars';
import classnames from 'classnames';
import _ from 'lodash';

import { Message } from '/components/lib/message';
import { ChatHeader } from '/components/lib/chat-header';
import { ChatInput } from '/components/lib/chat-input';

import { prettyShip, getMessageContent, isDMStation } from '/lib/util';

export class ChatScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
      message: "",
      pendingMessages: [],
      numPeople: 0
    };


    this.onScrollStop = this.onScrollStop.bind(this);
    this.buildMessage = this.buildMessage.bind(this);
    this.setPendingMessage = this.setPendingMessage.bind(this);

    this.scrollbarRef = React.createRef();
  }

  componentDidMount() {
    if (isDMStation(this.state.station)) {
      let cir = this.state.station.split("/")[1];
      this.props.api.hall({
        newdm: {
          sis: cir.split(".")
        }
      })
    }

    this.scrollIfLocked();
    this.updateNumPeople();
  }

  componentDidUpdate(prevProps, prevState) {
    const { props } = this;
    if ((props.match.params.ship != prevProps.match.params.ship) ||
      (props.match.params.station != prevProps.match.params.station)) {
      this.setState({
        station: props.match.params.ship + "/" + props.match.params.station,
        circle: props.match.params.station,
        host: props.match.params.ship,
        message: "",
        pendingMessages: [],
        numPeople: 0
      });
    }

    this.updateNumPeople();
    this.updateNumMessagesLoaded(prevProps, prevState);
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
    let station = prevProps.store.messages.stations[this.state.station] || [];
    let numMessages = station.length;

    if (numMessages > prevState.numMessages && this.scrollbarRef.current) {
      this.setState({
        numMessages: numMessages
      });

      this.scrollIfLocked();
    }
  }

  scrollIfLocked() {
    if (this.state.scrollLocked && this.scrollbarRef.current) {
      this.scrollbarRef.current.scrollToBottom();
    }
  }

  onScrollStop() {
    let scroll = this.scrollbarRef.current.getValues();

    this.setState({
      scrollLocked: (scroll.top === 1)
    });

    if (scroll.top === 0) {
      this.requestChatBatch();
    }
  }

  setPendingMessage(message) {
    this.setState({
      pendingMessages: this.state.pendingMessages.concat({...message, pending: true})
    });
  }

  buildMessage(msg) {
    let details = msg.printship ? null : getMessageContent(msg);

    if (msg.printship) {
      return (
        <a 
          className="vanilla hoverline text-600 text-mono" 
          href={prettyShip(msg.aut)[1]}>
          {prettyShip(`~${msg.aut}`)[0]}
        </a>
      );
    }
    return (
      <Message msg={msg} details={details} />
    );
  }

  render() {
    let messages = this.props.store.messages.stations[this.state.station] || [];
    messages = [...messages, ...this.state.pendingMessages];
    
    let chatMessages = messages.map(this.buildMessage);

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div style={{ flexBasis:72 }}>
          <ChatHeader title={this.state.circle} numPeople={this.state.numPeople} />
        </div>
        <div style={{ flexGrow: 1 }}>
          <Scrollbars
            ref={this.scrollbarRef}
            renderTrackHorizontal={props => <div style={{display: "none"}}/>}
            onScrollStop={this.onScrollStop}
            renderView={props => <div {...props} />}
            style={{ height: '100%' }}
            autoHide>
            {chatMessages}
          </Scrollbars>
        </div>
        <div style={{ flexBasis:112 }}>
          <ChatInput 
            api={this.props.api}
            store={this.props.store}
            station={this.state.station}
            circle={this.state.circle}
            scrollbarRef={this.scrollbarRef}
            setPendingMessage={this.setPendingMessage}
            placeholder='Message...' />
        </div>
      </div>
    )
  }
}

