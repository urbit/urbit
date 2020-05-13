import React from 'react';
import { Route } from 'react-router-dom';

import Api from './api';
import Store from './store';
import Subscription from './subscription';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { Sidebar } from './components/sidebar';
import { ChatScreen } from './components/chat';
import { MemberScreen } from './components/member';
import { SettingsScreen } from './components/settings';
import { NewScreen } from './components/new';
import { JoinScreen } from './components/join';
import { NewDmScreen } from './components/new-dm';

export default class ChatApp extends React.Component {
  constructor(props) {
    super(props);
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));

    this.state = this.store.state;
    this.totalUnreads = 0;
    this.resetControllers();
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    window.title = 'OS1 - Chat';
    // preload spinner asset
    new Image().src = '/~chat/img/Spinner.png';

    this.store.clear();
    const channel = new this.props.channel();
    this.api = new Api(this.props.ship, channel, this.store);

    this.subscription = new Subscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.resetControllers();
  }

  render() {
    const { state, props } = this;

    const messagePreviews = {};
    const unreads = {};
    let totalUnreads = 0;
    Object.keys(state.inbox).forEach((stat) => {
      const envelopes = state.inbox[stat].envelopes;

      if (envelopes.length === 0) {
        messagePreviews[stat] = false;
      } else {
        messagePreviews[stat] = envelopes[0];
      }

      const unread = Math.max(state.inbox[stat].config.length - state.inbox[stat].config.read, 0);
      unreads[stat] = Boolean(unread);
      if (unread) {
        totalUnreads += unread;
      }
    });

    if (totalUnreads !== this.totalUnreads) {
      document.title = totalUnreads > 0 ? `Chat - (${totalUnreads})` : 'Chat';
      this.totalUnreads = totalUnreads;
    }

    const invites = state.invites ? state.invites : { '/chat': {}, '/contacts': {} };

    const contacts = state.contacts ? state.contacts : {};
    const associations = state.associations ? state.associations : { chat: {}, contacts: {} };
    const s3 = state.s3 ? state.s3 : {};

    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

    const renderChannelSidebar = (props, station) => (
      <Sidebar
        inbox={state.inbox}
        messagePreviews={messagePreviews}
        associations={associations}
        selectedGroups={selectedGroups}
        contacts={contacts}
        invites={invites['/chat'] || {}}
        unreads={unreads}
        api={this.api}
        station={station}
        {...props}
      />
    );

    return (
      <div>
        <Route
          exact
          path="/~chat"
          render={(props) => {
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
          render={(props) => {
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
                  api={this.api}
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
          render={(props) => {
            return (
              <Skeleton
                associations={associations}
                invites={invites}
                sidebarHideOnMobile={true}
                sidebar={renderChannelSidebar(props)}
                sidebarShown={state.sidebarShown}
              >
                <NewScreen
                  api={this.api}
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
          render={(props) => {
            let station =
              `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
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
                  api={this.api}
                  inbox={state.inbox}
                  autoJoin={station}
                  chatSynced={state.chatSynced || {}}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route
          exact
          path="/~chat/(popout)?/room/(~)?/:ship/:station+"
          render={(props) => {
            let station =
              `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
            if (sig) {
              station = '/~' + station;
            }
            const mailbox = state.inbox[station] || {
              config: {
                read: 0,
                length: 0
              },
              envelopes: []
            };

            let roomContacts = {};
            const associatedGroup =
              station in associations['chat'] &&
                'group-path' in associations.chat[station]
                ? associations.chat[station]['group-path']
                : '';

            if ((associations.chat[station]) && (associatedGroup in contacts)) {
              roomContacts = contacts[associatedGroup];
            }

            const association =
              station in associations['chat'] ? associations.chat[station] : {};

            const permission =
              station in state.permissions ? state.permissions[station] : {
                who: new Set([]),
                kind: 'white'
              };
            const popout = props.match.url.includes('/popout/');

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
                  api={this.api}
                  subscription={this.subscription}
                  read={mailbox.config.read}
                  length={mailbox.config.length}
                  envelopes={mailbox.envelopes}
                  inbox={state.inbox}
                  contacts={roomContacts}
                  permission={permission}
                  pendingMessages={state.pendingMessages}
                  s3={s3}
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
          render={(props) => {
            let station = `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
            if (sig) {
              station = '/~' + station;
            }

            const permission = state.permissions[station] || {
              kind: '',
              who: new Set([])
            };
            const popout = props.match.url.includes('/popout/');

            const association =
              station in associations['chat'] ? associations.chat[station] : {};

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
          render={(props) => {
            let station =
              `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
            if (sig) {
              station = '/~' + station;
            }

            const popout = props.match.url.includes('/popout/');

            const permission = state.permissions[station] || {
              kind: '',
              who: new Set([])
            };

            const association =
              station in associations['chat'] ? associations.chat[station] : {};

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
                  inbox={state.inbox}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                />
              </Skeleton>
            );
          }}
        />
      </div>
  );
  }
}
