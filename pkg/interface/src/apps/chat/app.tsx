import React from 'react';
import { Route, Switch } from 'react-router-dom';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { Sidebar } from './components/sidebar';
import { ChatScreen } from './components/chat';
import { MemberScreen } from './components/member';
import { SettingsScreen } from './components/settings';
import { NewScreen } from './components/new';
import { JoinScreen } from './components/join';
import { NewDmScreen } from './components/new-dm';
import { PatpNoSig } from '../../types/noun';
import GlobalApi from '../../api/global';
import { StoreState } from '../../store/type';
import GlobalSubscription from '../../subscription/global';

type ChatAppProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
};

export default class ChatApp extends React.Component<ChatAppProps, {}> {
  totalUnreads = 0;

  constructor(props) {
    super(props);
  }

  componentDidMount() {
    document.title = 'OS1 - Chat';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('chat');

    if (!this.props.sidebarShown) {
      this.props.api.local.sidebarToggle();
    }
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('chat');
  }

  render() {
    const { props } = this;

    const messagePreviews = {};
    const unreads = {};
    let totalUnreads = 0;

    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];
    const associations = props.associations
      ? props.associations
      : { chat: {}, contacts: {} };

    Object.keys(props.inbox).forEach((stat) => {
      const envelopes = props.inbox[stat].envelopes;

      if (envelopes.length === 0) {
        messagePreviews[stat] = false;
      } else {
        messagePreviews[stat] = envelopes[0];
      }

      const unread = Math.max(
        props.inbox[stat].config.length - props.inbox[stat].config.read,
        0
      );
      unreads[stat] = Boolean(unread);
      if (
        unread &&
        (selectedGroups.length === 0 ||
          selectedGroups
            .map((e) => {
              return e[0];
            })
            .includes(associations.chat?.[stat]?.['group-path']) ||
          associations.chat?.[stat]?.['group-path'].startsWith('/~/'))
      ) {
        totalUnreads += unread;
      }
    });

    if (totalUnreads !== this.totalUnreads) {
      document.title =
        totalUnreads > 0 ? `OS1 - Chat (${totalUnreads})` : 'OS1 - Chat';
      this.totalUnreads = totalUnreads;
    }

    const {
      invites,
      s3,
      sidebarShown,
      inbox,
      contacts,
      permissions,
      chatSynced,
      api,
      chatInitialized,
      pendingMessages
    } = props;

    const renderChannelSidebar = (props, station?) => (
      <Sidebar
        inbox={inbox}
        messagePreviews={messagePreviews}
        associations={associations}
        selectedGroups={selectedGroups}
        contacts={contacts}
        invites={invites['/chat'] || {}}
        unreads={unreads}
        api={api}
        station={station}
        {...props}
      />
    );

    return (
      <Switch>
        <Route
          exact
          path="/~chat"
          render={(props) => {
            return (
              <Skeleton
                associations={associations}
                invites={invites}
                chatHideonMobile={true}
                sidebarShown={sidebarShown}
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
                sidebarShown={sidebarShown}
              >
                <NewDmScreen
                  api={api}
                  inbox={inbox}
                  permissions={permissions || {}}
                  contacts={contacts || {}}
                  associations={associations.contacts}
                  chatSynced={chatSynced || {}}
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
                sidebarShown={sidebarShown}
              >
                <NewScreen
                  api={api}
                  inbox={inbox || {}}
                  permissions={permissions || {}}
                  contacts={contacts || {}}
                  associations={associations.contacts}
                  chatSynced={chatSynced || {}}
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
            let station = `/${props.match.params.ship}/${props.match.params.station}`;
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
                sidebarShown={sidebarShown}
              >
                <JoinScreen
                  api={api}
                  inbox={inbox}
                  autoJoin={station}
                  chatSynced={chatSynced || {}}
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
            let station = `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
            if (sig) {
              station = '/~' + station;
            }
            const mailbox = inbox[station] || {
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

            if (associations.chat[station] && associatedGroup in contacts) {
              roomContacts = contacts[associatedGroup];
            }

            const association =
              station in associations['chat'] ? associations.chat[station] : {};

            const permission =
              station in permissions
                ? permissions[station]
                : {
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
                sidebarShown={sidebarShown}
                sidebar={renderChannelSidebar(props, station)}
              >
                <ChatScreen
                  chatSynced={chatSynced || {}}
                  station={station}
                  association={association}
                  api={api}
                  read={mailbox.config.read}
                  length={mailbox.config.length}
                  envelopes={mailbox.envelopes}
                  inbox={inbox}
                  contacts={roomContacts}
                  permission={permission}
                  pendingMessages={pendingMessages}
                  s3={s3}
                  popout={popout}
                  sidebarShown={sidebarShown}
                  chatInitialized={chatInitialized}
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

            const permission = permissions[station] || {
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
                sidebarShown={sidebarShown}
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
                  permissions={permissions}
                  popout={popout}
                  sidebarShown={sidebarShown}
                />
              </Skeleton>
            );
          }}
        />
        <Route
          exact
          path="/~chat/(popout)?/settings/(~)?/:ship/:station+"
          render={(props) => {
            let station = `/${props.match.params.ship}/${props.match.params.station}`;
            const sig = props.match.url.includes('/~/');
            if (sig) {
              station = '/~' + station;
            }

            const popout = props.match.url.includes('/popout/');

            const permission = permissions[station] || {
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
                sidebarShown={sidebarShown}
                sidebar={renderChannelSidebar(props, station)}
              >
                <SettingsScreen
                  {...props}
                  station={station}
                  association={association}
                  permission={permission}
                  permissions={permissions || {}}
                  contacts={contacts || {}}
                  associations={associations.contacts}
                  api={api}
                  inbox={inbox}
                  popout={popout}
                  sidebarShown={sidebarShown}
                />
              </Skeleton>
            );
          }}
        />
      </Switch>
    );
  }
}
