import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { Route, Link } from "react-router-dom";
import { store } from "/store";


import { Message } from '/components/lib/message';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { ChatInput } from '/components/lib/chat-input';
import { deSig } from '/lib/util';


export class ChatScreen extends Component {
         constructor(props) {
           super(props);

           this.state = {
             station: `/${props.match.params.ship}/${props.match.params.station}`,
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

             clearInterval(this.updateReadInterval);

             this.setState(
               {
                 station: `/${props.match.params.ship}/${props.match.params.station}`,
                 scrollLocked: false
               },
               () => {
                 this.scrollToBottom();
                 this.updateReadInterval = setInterval(
                   this.updateReadNumber.bind(this),
                   1000
                 );
                 this.updateReadNumber();
               }
             );
           } else if (Object.keys(props.inbox).length === 0) {
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
           if (props.read < props.envelopes.length) {
             props.api.chat.read(state.station);
           }
         }

         askForMessages() {
           const { props, state } = this;

           if (
             state.numPages * 100 < props.envelopes.length - 400 ||
             this.hasAskedForMessages
           ) {
             return;
           }

           if (props.envelopes.length > 0) {
             let end = props.envelopes[0].number;
             if (end > 0) {
               let start = end - 400 > 0 ? end - 400 : 0;

               if (start === 0 && end === 1) {
                 return;
               }

               this.hasAskedForMessages = true;

               props.subscription.fetchMessages(start, end - 1, state.station);
             }
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
             messages = messages.slice(
               messages.length - 100 * state.numPages,
               messages.length
             );
           }

           let pendingMessages = props.pendingMessages.has(state.station)
             ? props.pendingMessages.get(state.station)
             : [];

           pendingMessages.map(function(value) {
             return (value.pending = true);
           });

           let reversedMessages = messages.concat(pendingMessages);
           reversedMessages = reversedMessages.reverse();

           reversedMessages = reversedMessages.map((msg, i) => {
             // Render sigil if previous message is not by the same sender
             let aut = ["author"];
             let renderSigil =
               _.get(reversedMessages[i + 1], aut) !==
               _.get(msg, aut, msg.author);
             let paddingTop = renderSigil;
             let paddingBot =
               _.get(reversedMessages[i - 1], aut) !==
               _.get(msg, aut, msg.author);

             return (
               <Message
                 key={msg.uid}
                 msg={msg}
                 renderSigil={renderSigil}
                 paddingTop={paddingTop}
                 paddingBot={paddingBot}
                 pending={!!msg.pending}
               />
             );
           });

           let group = Array.from(props.group.values());

           let popoutSwitcher = this.props.popout ? "dn-m dn-l dn-xl" : "dib-m dib-l dib-xl";
           let isinPopout = this.props.popout ? "popout/" : "";

           return (
             <div
               key={state.station}
               className="h-100 w-100 overflow-hidden flex flex-column">
               <div className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
                 style={{ height: "1rem" }}>
                 <Link to="/~chat/">{"⟵ All Chats"}</Link>
               </div>
               <div className="pl3 pt4 bb b--gray4 flex relative overflow-x-scroll flex-shrink-0"
                 style={{ height: 48 }}>
                 <a className="pointer flex-shrink-0"
                    onClick={() => {
                     store.setState(previousState => ({
                       sidebarShown: !previousState.sidebarShown
                     }));
                   }}>
                   <img className={`v-btm pr3 dn ` + popoutSwitcher}
                     src={
                       this.props.sidebarShown
                         ? "/~chat/img/ChatSwitcherLink.png"
                         : "/~chat/img/ChatSwitcherClosed.png"
                     }
                     height="16"
                     width="16"
                   />
                 </a>
                 <Link to={`/~chat/` + isinPopout + `room` + state.station}>
                   <h2
                     className="mono dib f7 fw4 v-top"
                     style={{ width: "max-content" }}>
                     {state.station.substr(1)}
                   </h2>
                 </Link>
                 <ChatTabBar
                   {...props}
                   station={state.station}
                   numPeers={group.length}
                   isOwner={deSig(props.match.params.ship) === window.ship}
                   popout={this.props.popout}
                 />
               </div>
               <div className="overflow-y-scroll pt3 pb2 flex flex-column-reverse"
                 style={{ height: "100%", resize: "vertical" }}
                 onScroll={this.onScroll}>
                 <div ref={el => {
                     this.scrollElement = el;
                   }}></div>
                 {reversedMessages}
               </div>
               <ChatInput
                 api={props.api}
                 numMsgs={lastMsgNum}
                 station={state.station}
                 owner={deSig(props.match.params.ship)}
                 permissions={props.permissions}
                 placeholder="Message..."
               />
             </div>
           );
         }
       }
