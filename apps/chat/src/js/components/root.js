import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import Mousetrap from 'mousetrap';
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { Sidebar } from '/components/sidebar';
import { ChatScreen } from '/components/chat';
import { MemberScreen } from '/components/member';
import { SettingsScreen } from '/components/settings';
import { NewScreen } from '/components/new';
import { LandingScreen } from '/components/landing';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));

    Mousetrap.bind(["command+c"], () => {
      props.history.push('/~chat/new');
      return false;
    });

    Mousetrap.bind(["command+h"], () => {
      window.location.replace('/');
      return false;
    });
  }

  render() {
    const { props, state } = this;

    let configs = !!state.configs ? state.configs : {};

    let circles = Object.keys(configs).filter((conf) => {
      if (!configs[conf]) {
        return false;
      }
      let cap = configs[conf].cap;
      return cap === 'dm' || cap === 'chatroom';
    });

    let messages = _.get(state, 'messages', {});
    let messagePreviews = {};
    Object.keys(messages).forEach((stat) => {
      let arr = messages[stat];
      if (arr.length === 0) {
        messagePreviews[stat] = false;
      } else {
        messagePreviews[stat] = arr[arr.length - 1];
      }
    });

    let unreads = {};
    circles.forEach((cir) => {
      if (cir in messages) {
        if (messages[cir].length === 0) {
          unreads[cir] = false;
        } else {
          let host = `~${window.ship}`;
          let circle = cir.split('/')[1];
          let internalStation = host + '/hall-internal-' + circle;

          if (internalStation in state.configs) {
            unreads[cir] = 
              state.configs[internalStation].red <=
              messages[cir][messages[cir].length - 1].num;
          } else {
            unreads[cir] =
              state.configs[cir].red <=
              messages[cir][messages[cir].length - 1].num;
          }
        }
      } else {
        unreads[cir] = false;
      }
    });

    let invites = _.get(state, 'messages', {});
    if (`~${window.ship}/i` in invites) {
      invites = invites[`~${window.ship}/i`];
    } else {
      invites = [];
    }

    let inviteConfig = false;
    if (`~${window.ship}/i` in configs) {
      inviteConfig = configs[`~${window.ship}/i`];
    }

    return (
      <BrowserRouter>
        <div>
        <Route exact path="/~chat"
          render={ (props) => {
            return (
              <Skeleton
                sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    unreads={unreads}
                    api={api}
                    inviteConfig={inviteConfig}
                    {...props}
                  />
                }>
                <div className="w-100 h-100 fr" style={{ flexGrow: 1 }}>
                  <div className="dt w-100 h-100">
                    <div className="dtc center v-mid w-100 h-100 bg-white">
                      <p className="tc">Cmd + N to start a new chat</p>
                    </div>
                  </div>
                </div>
              </Skeleton>
            );
          }} />
        <Route exact path="/~chat/new"
          render={ (props) => {
            return (
              <Skeleton
                sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    unreads={unreads}
                    api={api}
                    inviteConfig={inviteConfig}
                    {...props}
                  />
                }>
                <NewScreen 
                  api={api}
                  circles={circles}
                  {...props}
                />
              </Skeleton>
            );
          }} />
        <Route exact path="/~chat/join/:ship/:station"
          render={ (props) => {
            return (
              <Skeleton
                sidebar={
                 <Sidebar 
                   circles={circles}
                   messagePreviews={messagePreviews}
                   invites={invites}
                   unreads={unreads}
                   api={api}
                   inviteConfig={inviteConfig}
                   {...props}
                 />
                }>
                <LandingScreen
                  api={api}
                  configs={configs}
                  {...props}
                />
              </Skeleton>
            );
           }} />
         <Route exact path="/~chat/:ship/:station"
           render={ (props) => {
             let station = 
               props.match.params.ship
               + "/" +
               props.match.params.station;
             let messages = state.messages[station] || [];
             return (
               <Skeleton
                 sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    unreads={unreads}
                    api={api}
                    inviteConfig={inviteConfig}
                    {...props}
                  />
                 }>
                 <ChatScreen
                   api={api}
                   configs={configs}
                   messages={messages}
                   peers={state.peers}
                   subscription={subscription}
                   {...props}
                 />
               </Skeleton>
             );
           }} />
         <Route exact path="/~chat/:ship/:station/members"
           render={ (props) => {
             return (
               <Skeleton
                 sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    unreads={unreads}
                    api={api}
                    inviteConfig={inviteConfig}
                    {...props}
                  />
                 }>
                 <MemberScreen
                   {...props} 
                   api={api}
                   peers={state.peers}
                 />
               </Skeleton>
             );
           }} />
         <Route exact path="/~chat/:ship/:station/settings"
           render={ (props) => {
             return (
               <Skeleton
                 sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    unreads={unreads}
                    api={api}
                    inviteConfig={inviteConfig}
                    {...props}
                  />
                 }>
                 <SettingsScreen 
                   {...props}
                   api={api}
                   peers={state.peers}
                   circles={state.circles}
                 />
               </Skeleton>
             );
           }} />
        </div>
      </BrowserRouter>
    )
  }
}

