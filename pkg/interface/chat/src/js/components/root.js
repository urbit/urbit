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
import { NewDmScreen } from '/components/new-dm';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    this.totalUnreads = 0;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    //preload spinner asset
    new Image().src = "/~chat/img/Spinner.png";
  }

  render() {
    const { props, state } = this;

    let messagePreviews = {};
    let unreads = {};
    let totalUnreads = 0;
    Object.keys(state.inbox).forEach((stat) => {
      let envelopes = state.inbox[stat].envelopes;

      if (envelopes.length === 0) {
        messagePreviews[stat] = false;
      } else {
        messagePreviews[stat] = envelopes[0];
      }

      const unread = Math.max(state.inbox[stat].config.length - state.inbox[stat].config.read, 0)
      unreads[stat] = !!unread;
      if(unread) {
        totalUnreads += unread;
      }
    });
    if(totalUnreads !== this.totalUnreads) {
      document.title = totalUnreads > 0 ? `Chat - (${totalUnreads})` : 'Chat';
      this.totalUnreads = totalUnreads;
    }

    let invites = !!state.invites ? state.invites : {'/chat': {}, '/contacts': {}};

    let contacts = !!state.contacts ? state.contacts : {};
    let associations = !!state.associations ? state.associations : {chat: {}, contacts: {}};

    const renderChannelSidebar = (props, station) => (
      <Sidebar
        inbox={state.inbox}
        messagePreviews={messagePreviews}
        associations={associations}
        selectedGroups={state.selectedGroups}
        contacts={contacts}
        invites={invites["/chat"] || {}}
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
                  associations={associations}
                  invites={invites}
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
            path="/~chat/new/dm/:ship"
            render={props => {
              const ship = props.match.params.ship;

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
                  sidebar={renderChannelSidebar(props)}
                  sidebarShown={state.sidebarShown}
                >
                  <NewDmScreen
                    api={api}
                    inbox={state.inbox || {}}
                    permissions={state.permissions || {}}
                    contacts={state.contacts || {}}
                    associations={associations.contacts}
                    chatSynced={state.chatSynced || {}}
                    autoCreate={ship}
                    {...props}
                  />
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
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
                  sidebar={renderChannelSidebar(props)}
                  sidebarShown={state.sidebarShown}
                >
                  <NewScreen
                    api={api}
                    inbox={state.inbox || {}}
                    permissions={state.permissions || {}}
                    contacts={state.contacts || {}}
                    associations={associations.contacts}
                    chatSynced={state.chatSynced || {}}
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
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
                  sidebar={renderChannelSidebar(props)}
                  sidebarShown={state.sidebarShown}
                >
                  <JoinScreen
                    api={api}
                    inbox={state.inbox}
                    autoJoin={station}
                    chatSynced={state.chatSynced || {}}
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

              let permission =
                station in state.permissions ? state.permissions[station] : {
                  who: new Set([]),
                  kind: 'white'
                };
              let popout = props.match.url.includes("/popout/");

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  sidebar={renderChannelSidebar(props, station)}
                >
                  <ChatScreen
                    chatSynced={state.chatSynced}
                    station={station}
                    association={association}
                    api={api}
                    subscription={subscription}
                    read={mailbox.config.read}
                    length={mailbox.config.length}
                    envelopes={mailbox.envelopes}
                    inbox={state.inbox}
                    contacts={roomContacts}
                    permission={permission}
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
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
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

              let popout = props.match.url.includes("/popout/");

              let permission = state.permissions[station] || {
                kind: "",
                who: new Set([])
              };

              let association =
                station in associations["chat"] ? associations.chat[station] : {};

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  sidebarHideOnMobile={true}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  sidebar={renderChannelSidebar(props, station)}
                >
                  <SettingsScreen
                    {...props}
                    station={station}
                    association={association}
                    permission={permission}
                    permissions={state.permissions || {}}
                    contacts={state.contacts || {}}
                    associations={associations.contacts}
                    api={api}
                    station={station}
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

