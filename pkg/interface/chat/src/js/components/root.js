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

      unreads[stat] =
        state.inbox[stat].config.length > state.inbox[stat].config.read;
    });
    
    let invites = '/chat' in state.invites ?
      state.invites['/chat'] : {};

    let contacts = !!state.contacts ? state.contacts : {};
    let associations = !!state.associations ? state.associations : {chat: {}, contacts: {}};

    const renderChannelSidebar = (props, station) => (
      <Sidebar
        inbox={state.inbox}
        messagePreviews={messagePreviews}
        associations={associations}
        contacts={contacts}
        invites={invites}
        unreads={unreads}
        api={api}
        station={station}
        {...props}
      />
    );

    return (
      <BrowserRouter>
        <div>
          <Route
            exact
            path="/~chat"
            render={props => {
              return (
                <Skeleton
                  chatHideonMobile={true}
                  sidebarShown={state.sidebarShown}
                  sidebar={renderChannelSidebar(props)}
                >
                  <div className="h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d">
                    <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                      <p className="f8 pt3 gray2 w-100 h-100 dtc v-mid tc">
                        Select, create, or join a chat to begin.
                      </p>
                    </div>
                  </div>
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~chat/new"
            render={props => {
              return (
                <Skeleton
                  sidebarHideOnMobile={true}
                  spinner={state.spinner}
                  sidebar={renderChannelSidebar(props)}
                  sidebarShown={state.sidebarShown}
                >
                  <NewScreen
                    api={api}
                    inbox={state.inbox || {}}
                    groups={state.groups || {}}
                    contacts={state.contacts || {}}
                    {...props}
                  />
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~chat/join/(~)?/:ship?/:station?"
            render={props => {
              let station =
                `/${props.match.params.ship}/${props.match.params.station}`;
              let sig = props.match.url.includes("/~/");
              if (sig) {
                station = '/~' + station;
              }


              return (
                <Skeleton
                  spinner={state.spinner}
                  sidebarHideOnMobile={true}
                  sidebar={renderChannelSidebar(props)}
                  sidebarShown={state.sidebarShown}
                >
                  <JoinScreen
                    api={api}
                    inbox={state.inbox}
                    autoJoin={station}
                    {...props} />
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~chat/(popout)?/room/(~)?/:ship/:station+"
            render={props => {
              let station =
                `/${props.match.params.ship}/${props.match.params.station}`;
              let sig = props.match.url.includes("/~/");
              if (sig) {
                station = '/~' + station;
              }
              let mailbox = state.inbox[station] || {
                config: {
                  read: 0,
                  length: 0
                },
                envelopes: []
              };

              let roomContacts = {};
              let associatedGroup =
                station in associations["chat"] &&
                "group-path" in associations.chat[station]
                  ? associations.chat[station]["group-path"]
                  : "";

              if ((associations.chat[station]) && (associatedGroup in contacts)) {
                roomContacts = contacts[associatedGroup]
              }

              let association =
                station in associations["chat"] ? associations.chat[station] : {};


              let group = state.groups[station] || new Set([]);
              let popout = props.match.url.includes("/popout/");

              return (
                <Skeleton
                  sidebarHideOnMobile={true}
                  spinner={state.spinner}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  sidebar={renderChannelSidebar(props, station)}
                >
                  <ChatScreen
                    station={station}
                    association={association}
                    api={api}
                    subscription={subscription}
                    read={mailbox.config.read}
                    length={mailbox.config.length}
                    envelopes={mailbox.envelopes}
                    inbox={state.inbox}
                    group={group}
                    contacts={roomContacts}
                    permissions={state.permissions}
                    pendingMessages={state.pendingMessages}
                    popout={popout}
                    sidebarShown={state.sidebarShown}
                    chatInitialized={state.chatInitialized}
                    {...props}
                  />
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~chat/(popout)?/members/(~)?/:ship/:station+"
            render={props => {
              let station = `/${props.match.params.ship}/${props.match.params.station}`;
              let sig = props.match.url.includes("/~/");
              if (sig) {
                station = '/~' + station;
              }

              let permission = state.permissions[station] || {
                kind: "",
                who: new Set([])
              };
              let popout = props.match.url.includes("/popout/");

              let association =
                station in associations["chat"] ? associations.chat[station] : {};

              return (
                <Skeleton
                  sidebarHideOnMobile={true}
                  spinner={state.spinner}
                  sidebarShown={state.sidebarShown}
                  popout={popout}
                  sidebar={renderChannelSidebar(props, station)}
                >
                  <MemberScreen
                    {...props}
                    api={api}
                    station={station}
                    association={association}
                    permission={permission}
                    contacts={contacts}
                    permissions={state.permissions}
                    popout={popout}
                    sidebarShown={state.sidebarShown}
                  />
                </Skeleton>
              );
            }}
          />
          <Route
            exact
            path="/~chat/(popout)?/settings/(~)?/:ship/:station+"
            render={props => {
              let station =
                `/${props.match.params.ship}/${props.match.params.station}`;
              let sig = props.match.url.includes("/~/");
              if (sig) {
                station = '/~' + station;
              }
              let group = state.groups[station] || new Set([]);

              let popout = props.match.url.includes("/popout/");

              let association =
                station in associations["chat"] ? associations.chat[station] : {};

              return (
                <Skeleton
                  sidebarHideOnMobile={true}
                  spinner={state.spinner}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  sidebar={renderChannelSidebar(props, station)}
                >
                  <SettingsScreen
                    {...props}
                    station={station}
                    association={association}
                    api={api}
                    station={station}
                    group={group}
                    inbox={state.inbox}
                    popout={popout}
                    sidebarShown={state.sidebarShown}
                  />
                </Skeleton>
              );
            }}
          />
        </div>
      </BrowserRouter>
    );
  }
}

