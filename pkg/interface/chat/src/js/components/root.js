import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
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
import { JoinScreen } from '/components/join';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
    this.setSpinner = this.setSpinner.bind(this);
  }

  setSpinner(spinner) {
    this.setState({
      spinner
    });
  }

  render() {
    const { props, state } = this;

    let messagePreviews = {};
    let unreads = {};
    Object.keys(state.inbox).forEach((stat) => {
      let envelopes = state.inbox[stat].envelopes;
      
      if (envelopes.length === 0) {
        messagePreviews[stat] = false;
      } else {
        messagePreviews[stat] = envelopes[envelopes.length - 1];
      }

      unreads[stat] = envelopes.length > state.inbox[stat].config.read;
    });
    
    let inviteConfig = false;

    const renderChannelSidebar = (props) => (
      <Sidebar
        inbox={state.inbox}
        messagePreviews={messagePreviews}
        invites={[]}
        unreads={unreads}
        api={api}
        inviteConfig={inviteConfig}
        {...props}
      />
    );

    return (
      <BrowserRouter>
        <div>
        <Route exact path="/~chat"
          render={ (props) => {
            return (
              <Skeleton sidebar={renderChannelSidebar(props)}>
                <div className="h-100 w-100 overflow-x-hidden flex flex-column">
                  <div className="pl3 pr3 pt2 pb3">
                    <h2>Home</h2>
                    <p className="body-regular-400 pt3">
                      Select a chat from the sidebar
                      or <Link to="/~chat/new">create a new one</Link>.
                    </p>
                  </div>
                </div>
              </Skeleton>
            );
          }} />
        <Route exact path="/~chat/new"
          render={ (props) => {
            return (
              <Skeleton
                spinner={this.state.spinner}
                sidebar={renderChannelSidebar(props)}>
                <NewScreen
                  setSpinner={this.setSpinner}
                  api={api}
                  inbox={state.inbox || {}}
                  {...props}
                />
              </Skeleton>
            );
          }} />
        <Route exact path="/~chat/join/:ship/:station"
          render={ (props) => {
            return (
              <Skeleton sidebar={renderChannelSidebar(props)}>
                <JoinScreen
                  api={api}
                  inbox={state.inbox}
                  {...props}
                />
              </Skeleton>
            );
           }} />
         <Route exact path="/~chat/room/:ship/:station"
           render={ (props) => {
             let station =
               `/${props.match.params.ship}/${props.match.params.station}`;
             let mailbox = state.inbox[station] || {
               config: {
                 read: -1,
                 length: 0
               },
               envelopes: []
             };

             let write = state.groups[`/chat${station}/write`] || new Set([]);

             return (
               <Skeleton sidebar={renderChannelSidebar(props) }>
                 <ChatScreen
                   api={api}
                   subscription={subscription}
                   read={mailbox.config.read}
                   envelopes={mailbox.envelopes}
                   inbox={state.inbox}
                   group={write}
                   permissions={state.permissions}
                   pendingMessages={state.pendingMessages}
                   {...props}
                 />
               </Skeleton>
             );
           }} />
         <Route exact path="/~chat/room/:ship/:station/members"
           render={ (props) => {
             let station =
               `/${props.match.params.ship}/${props.match.params.station}`;
             let read = state.permissions[`/chat${station}/read`] || {
               kind: '',
               who: new Set([])
             };
             let write = state.permissions[`/chat${station}/write`] || {
               kind: '',
               who: new Set([])
             };

             return (
               <Skeleton sidebar={renderChannelSidebar(props) }>
                 <MemberScreen
                   {...props}
                   api={api}
                   read={read}
                   write={write}
                   permissions={state.permissions}
                 />
               </Skeleton>
             );
           }} />
         <Route exact path="/~chat/room/:ship/:station/settings"
           render={ (props) => {
             let station =
               `/${props.match.params.ship}/${props.match.params.station}`;
             let write = state.groups[`/chat${station}/write`] || new Set([]);

             return (
               <Skeleton
                 spinner={this.state.spinner}
                 sidebar={renderChannelSidebar(props) }>
                 <SettingsScreen
                   {...props}
                   setSpinner={this.setSpinner}
                   api={api}
                   group={write}
                   inbox={state.inbox}
                 />
               </Skeleton>
             );
           }} />
        </div>
      </BrowserRouter>
    )
  }
}

