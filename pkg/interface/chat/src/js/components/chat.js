import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { Route, Link } from "react-router-dom";
import { store } from "/store";

import { ResubscribeElement } from '/components/lib/resubscribe-element';
import { Message } from '/components/lib/message';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';
import { deSig } from '/lib/util';


export class ChatScreen extends Component {
  constructor(props) {
    super(props);
 
    this.state = {
      numPages: 1,
      scrollLocked: false
    };
 
    this.hasAskedForMessages = false;
    this.onScroll = this.onScroll.bind(this);
 
    this.updateReadInterval = setInterval(
      this.updateReadNumber.bind(this),
      1000
    );
  }
 
  componentDidMount() {
    this.updateReadNumber();
    this.askForMessages();
  }
 
  componentWillUnmount() {
    if (this.updateReadInterval) {
      clearInterval(this.updateReadInterval);
      this.updateReadInterval = null;
    }
  }
 
  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
 
    if (
      prevProps.match.params.station !== props.match.params.station ||
      prevProps.match.params.ship !== props.match.params.ship
    ) {
      this.hasAskedForMessages = false;
      
      if (props.envelopes.length < 100) {
        this.askForMessages();
      }
 
      clearInterval(this.updateReadInterval);
 
      this.setState(
        { scrollLocked: false },
        () => {
          this.scrollToBottom();
          this.updateReadInterval = setInterval(
            this.updateReadNumber.bind(this),
            1000
          );
          this.updateReadNumber();
        }
      );
    } else if (props.chatInitialized &&
               !(props.station in props.inbox) &&
               !(props.station in props.chatSynced) ) {

      props.history.push("/~chat");
    } else if (
      props.envelopes.length - prevProps.envelopes.length >=
      200
    ) {
      this.hasAskedForMessages = false;
    }
  }
 
  updateReadNumber() {
    const { props, state } = this;
    if (props.read < props.length) {
      props.api.chat.read(props.station);
    }
  }
 
  askForMessages() {
    const { props, state } = this;

    if (props.envelopes.length === 0) {
      setTimeout(() => {
        this.askForMessages();
      }, 500);
      return;
    }

    if (
      props.envelopes.length >= props.length ||
      this.hasAskedForMessages ||
      props.length <= 0
    ) {
      return;
    }

    let start = props.envelopes[props.envelopes.length - 1].number;
    if (start > 0) {
      let end = start + 200 < props.length ? start + 200 : props.length;
      this.hasAskedForMessages = true;
      props.subscription.fetchMessages(start, end, props.station);
    }
  }
 
  scrollToBottom() {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView({ behavior: "smooth" });
    }
  }
 
  onScroll(e) {
    if (
      navigator.userAgent.includes("Safari") &&
      navigator.userAgent.includes("Chrome")
    ) {
      // Google Chrome
      if (e.target.scrollTop === 0) {
        this.setState(
          {
            numPages: this.state.numPages + 1,
            scrollLocked: true
          },
          () => {
            this.askForMessages();
          }
        );
      } else if (
        e.target.scrollHeight - Math.round(e.target.scrollTop) ===
        e.target.clientHeight
      ) {
        this.setState({
          numPages: 1,
          scrollLocked: false
        });
      }
    } else if (navigator.userAgent.includes("Safari")) {
      // Safari
      if (e.target.scrollTop === 0) {
        this.setState({
          numPages: 1,
          scrollLocked: false
        });
      } else if (
        e.target.scrollHeight + Math.round(e.target.scrollTop) <=
        e.target.clientHeight + 10
      ) {
        this.setState(
          {
            numPages: this.state.numPages + 1,
            scrollLocked: true
          },
          () => {
            this.askForMessages();
          }
        );
      }
    } else {
      console.log("Your browser is not supported.");
    }
  }
 
  render() {
    const { props, state } = this;
 
    let messages = props.envelopes.slice(0);
    let lastMsgNum = messages.length > 0 ? messages.length : 0;
 
    if (messages.length > 100 * state.numPages) {
      messages = messages.slice(0, 100 * state.numPages);
    }
 
    let pendingMessages = props.pendingMessages.has(props.station)
      ? props.pendingMessages.get(props.station).reverse()
      : [];
 
    pendingMessages.map(function(value) {
      return (value.pending = true);
    });
 
    let messageElements = pendingMessages.concat(messages).map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      let aut = ["author"];
      let renderSigil =
        _.get(messages[i + 1], aut) !==
        _.get(msg, aut, msg.author);
      let paddingTop = renderSigil;
      let paddingBot =
        _.get(messages[i - 1], aut) !==
        _.get(msg, aut, msg.author);
 
      return (
        <Message
          key={msg.uid}
          msg={msg}
          contacts={props.contacts}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot}
          pending={!!msg.pending}
        />
      );
    });
 
    let group = Array.from(props.permission.who.values());
 
    const isinPopout = props.popout ? "popout/" : "";
 
    let ownerContact = (window.ship in props.contacts)
      ? props.contacts[window.ship] : false;
 
    let title = props.station.substr(1);
 
    if (props.association && "metadata" in props.association) {
      title =
        props.association.metadata.title !== ""
          ? props.association.metadata.title
          : props.station.substr(1);
    }
 
    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <div
          className={"pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative" +
          "overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"}
          style={{ height: 48 }}>
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
          />
          <Link to={`/~chat/` + isinPopout + `room` + props.station}
          className="pt2 white-d">
            <h2
              className={"dib f9 fw4 lh-solid v-top " +
              ((title === props.station.substr(1)) ? "mono" : "")}
              style={{ width: "max-content" }}>
              {title}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            numPeers={group.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
            api={props.api}
          />
        </div>
        <div
          className="overflow-y-scroll bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
          style={{ height: "100%", resize: "vertical" }}
          onScroll={this.onScroll}>
          <div
            ref={el => {
              this.scrollElement = el;
            }}></div>
            { (
                !(props.station in props.chatSynced) &&
                (messages.length > 0)
              ) ? (
                  <ResubscribeElement
                    api={props.api}
                    host={props.match.params.ship}
                    station={props.station} />
                ) : (<div/>)
            }
            {messageElements}
        </div>
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={props.station}
          owner={deSig(props.match.params.ship)}
          ownerContact={ownerContact}
          envelopes={props.envelopes}
          contacts={props.contacts}
          placeholder="Message..."
        />
      </div>
    );
  }
}
