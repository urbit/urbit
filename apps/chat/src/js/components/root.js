import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import Mousetrap from 'mousetrap';
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { Sidebar } from '/components/sidebar';
import { ChatScreen } from '/components/chat';
import { MemberScreen } from '/components/member';
import { SettingsScreen } from '/components/settings';
import { NewScreen } from '/components/new';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));

    Mousetrap.bind(["mod+n"], () => {
      props.history.push('/~chat/new');
      return false;
    });
  }

  render() {
    let configs = !!this.state.configs ? this.state.configs : {};
    let circles = Object.keys(configs).filter((conf) => {
      let cap = configs[conf].cap;
      return cap === 'dm' || cap === 'chatroom';
    });

    let messages = _.get(this.state, 'messages', {});
    let messagePreviews = {};
    Object.keys(messages).forEach((stat) => {
      let arr = messages[stat];
      if (arr.length === 0) {
        messagePreviews[stat] = null;
      } else {
        messagePreviews[stat] = arr[arr.length - 1];
      }
    });

    let invites = _.get(this.state, 'messages', {});
    if (`~${window.ship}/i` in invites) {
      invites = invites[`~${window.ship}/i`];
    } else {
      invites = [];
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
                    api={api}
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
                    api={api}
                    {...props}
                  />
                }>
                <NewScreen 
                  api={api}
                  {...props}
                />
              </Skeleton>
            );
         }} />
         <Route exact path="/~chat/:ship/:station"
           render={ (props) => {
             return (
               <Skeleton
                 sidebar={
                  <Sidebar 
                    circles={circles}
                    messagePreviews={messagePreviews}
                    invites={invites}
                    api={api}
                    {...props}
                  />
                 }>
                 <ChatScreen
                   api={api}
                   configs={configs}
                   messages={this.state.messages}
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
                    api={api}
                    {...props}
                  />
                 }>
                 <MemberScreen
                   {...props} 
                   api={api}
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
                    api={api}
                    {...props}
                  />
                 }>
                 <SettingsScreen 
                   {...props}
                   api={api}
                   store={store} />
               </Skeleton>
             );
           }} />
        </div>
      </BrowserRouter>
    )
  }
}

