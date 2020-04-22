import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';
import moment from 'moment';

import { Route, Link } from "react-router-dom";
import { store } from "/store";

import { ResubscribeElement } from '/components/lib/resubscribe-element';
import { BacklogElement } from '/components/lib/backlog-element';
import { Message } from '/components/lib/message';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';
import { UnreadNotice } from '/components/lib/unread-notice';
import { deSig } from '/lib/util';

function getNumPending(props) {
  const result = props.pendingMessages.has(props.station)
    ? props.pendingMessages.get(props.station).length
    : 0;
  return result;
}

export class ChatScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      numPages: 1,
      scrollLocked: false,
      // only for FF
      lastScrollHeight: null,
      scrollBottom: true
    };

    this.hasAskedForMessages = false;
    this.lastNumPending = 0;

    this.scrollContainer = null;
    this.onScroll = this.onScroll.bind(this);

    this.unreadMarker = null;

    moment.updateLocale('en', {
      calendar: {
        sameDay: '[Today]',
        nextDay: '[Tomorrow]',
        nextWeek: 'dddd',
        lastDay: '[Yesterday]',
        lastWeek: '[Last] dddd',
        sameElse: 'DD/MM/YYYY'
      }
    });

  }

  componentDidMount() {
    this.askForMessages();
    this.scrollToBottom();
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

      this.setState(
        { scrollLocked: false },
        () => {
          this.scrollToBottom();
        }
      );
    } else if (props.chatInitialized &&
               !(props.station in props.inbox) &&
               (!!props.chatSynced && !(props.station in props.chatSynced))) {

      props.history.push("/~chat");
    } else if (
      props.envelopes.length >= prevProps.envelopes.length + 10
    ) {
      this.hasAskedForMessages = false;
    }

    // FF logic
    if (
      navigator.userAgent.includes("Firefox") &&
      (props.length !== prevProps.length ||
       props.envelopes.length !== prevProps.envelopes.length ||
       getNumPending(props) !== this.lastNumPending ||
       state.numPages !== prevState.numPages)
    ) {
      if(state.scrollBottom) {
        setTimeout(() => {
          this.scrollToBottom();
        })
      } else {
        this.recalculateScrollTop();
      }

      this.lastNumPending = getNumPending(props);
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

    let start =
      props.length - props.envelopes[props.envelopes.length - 1].number;
    if (start > 0) {
      let end = start + 300 < props.length ? start + 300 : props.length;
      this.hasAskedForMessages = true;
      props.subscription.fetchMessages(start + 1, end, props.station);
    }
  }

  scrollToBottom() {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView();
    }
  }

  // Restore chat position on FF when new messages come in
  recalculateScrollTop() {
    if(!this.scrollContainer) {
      return;
    }

    const { lastScrollHeight } = this.state;
    let target = this.scrollContainer;
    let newScrollTop = this.scrollContainer.scrollHeight - lastScrollHeight;
    if(target.scrollTop !== 0 || newScrollTop === target.scrollTop) {
      return;
    }
    target.scrollTop = target.scrollHeight - lastScrollHeight;

  }

  onScroll(e) {
    if (
      (navigator.userAgent.includes("Safari") &&
      navigator.userAgent.includes("Chrome")) ||
      navigator.userAgent.includes("Firefox")
    ) {
      // Google Chrome and Firefox
      if (e.target.scrollTop === 0) {

        // Save scroll position for FF
        if (navigator.userAgent.includes('Firefox')) {

          this.setState({
            lastScrollHeight: e.target.scrollHeight
          })
        }
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
          scrollLocked: false,
          scrollBottom: true
        });
      } else if (navigator.userAgent.includes('Firefox')) {
        this.setState({ scrollBottom: false });
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
    if(!!this.unreadMarker) {
      if(
        !navigator.userAgent.includes('Firefox') &&
         e.target.scrollHeight - e.target.scrollTop - (e.target.clientHeight * 1.5) + this.unreadMarker.offsetTop > 50
      ) {
        this.props.api.chat.read(this.props.station);
      } else if(navigator.userAgent.includes('Firefox') &&
        this.unreadMarker.offsetTop - e.target.scrollTop - (e.target.clientHeight / 2) > 0
      ) {
        this.props.api.chat.read(this.props.station);
      }


    }
  }

  chatWindow(unread) {

    // Replace with just the "not Firefox" implementation
    // when Firefox #1042151 is patched.

    const { props, state } = this;

    let messages = props.envelopes.slice(0);
    let lastMsgNum = messages.length > 0 ? messages.length : 0;

    if (messages.length > 100 * state.numPages) {
      messages = messages.slice(0, 100 * state.numPages);
    }

    let pendingMessages = props.pendingMessages.has(props.station)
      ? props.pendingMessages.get(props.station)
      : [];


    pendingMessages.map(function (value) {
      return (value.pending = true);
    });

 
    messages = pendingMessages.concat(messages);

    let messageElements = messages.map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      let aut = ["author"];
      let renderSigil =
        _.get(messages[i + 1], aut) !==
        _.get(msg, aut, msg.author);
      let paddingTop = renderSigil;
      let paddingBot =
        _.get(messages[i - 1], aut) !==
        _.get(msg, aut, msg.author);

      let when = ['when'];
      let dayBreak =
          moment(_.get(messages[i+1], when)).format('YYYY.MM.DD')  !==
          moment(_.get(messages[i], when)).format('YYYY.MM.DD');
 
      const messageElem = (
        <Message
          key={msg.uid}
          msg={msg}
          contacts={props.contacts}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot}
          pending={!!msg.pending}
          group={props.association}
        />
      );
      if(unread > 0 && i === unread) {
        return (
          <>
            {messageElem}
            <div key={'unreads'+ msg.uid} ref={ref => (this.unreadMarker = ref)} className="mv2 green2 flex items-center f9">
              <hr className="ma0 w2 b--green2 bt-0" />
              <p className="mh4">
                New messages below
              </p>
              <hr className="ma0 flex-grow-1 b--green2 bt-0" />
              { dayBreak && (
                 <p className="gray2 mh4">
                   {moment(_.get(messages[i], when)).calendar()}
                 </p>
              )}
              <hr style={{ width: 'calc(50% - 48px)' }} className="b--green2 ma0 bt-0"/>
            </div>
          </>
        );
      } else if(dayBreak) {
        return (
          <>
            {messageElem}
            <div key={'daybreak' + msg.uid} className="pv3 gray2 b--gray2 flex items-center justify-center f9 ">
              <p>
                {moment(_.get(messages[i], when)).calendar()}
              </p>
            </div>
          </>
        );
      } else {
        return messageElem;
      }
    });

    if (navigator.userAgent.includes("Firefox")) {
      return (
        <div className="relative overflow-y-scroll h-100" onScroll={this.onScroll} ref={e => { this.scrollContainer = e; }}>
          <div
            className="bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
            style={{ resize: "vertical" }}
          >
            <div
              ref={el => {
                this.scrollElement = el;
              }}></div>
            {(props.chatInitialized &&
              !(props.station in props.inbox)) && (
                  <BacklogElement />
            )}
            {(
              props.chatSynced &&
              !(props.station in props.chatSynced) &&
              (messages.length > 0)
            ) ? (
                <ResubscribeElement
                  api={props.api}
                  host={props.match.params.ship}
                  station={props.station} />
              ) : (<div />)
            }
            {messageElements}
          </div>
        </div>
      )}
    else {
      return (
        <div
          className="overflow-y-scroll bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse relative"
          style={{ height: "100%", resize: "vertical" }}
          onScroll={this.onScroll}
        >
          <div
            ref={el => {
              this.scrollElement = el;
            }}></div>
          {(props.chatInitialized &&
            !(props.station in props.inbox)) && (
                <BacklogElement />
          )}
          {(
            props.chatSynced &&
            !(props.station in props.chatSynced) &&
            (messages.length > 0)
          ) ? (
              <ResubscribeElement
                api={props.api}
                host={props.match.params.ship}
                station={props.station} />
            ) : (<div />)
          }
          {messageElements}
        </div>
      )}
  }

  render() {
    const { props, state } = this;

    let messages = props.envelopes.slice(0);

    let lastMsgNum = messages.length > 0 ? messages.length : 0;

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

    const unread = props.length - props.read;

    const unreadMsg = unread > 0 && messages[unread - 1];

    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column relative">
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
        { !!unreadMsg && (
          <UnreadNotice
            unread={unread}
            unreadMsg={unreadMsg}
            onRead={() => props.api.chat.read(props.station)}
          />
        ) }
        {this.chatWindow(unread)}
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
