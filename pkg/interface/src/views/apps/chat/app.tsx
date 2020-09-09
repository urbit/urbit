import React from 'react';
import { Route, Switch } from 'react-router-dom';
import Helmet from 'react-helmet';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { Sidebar } from './components/sidebar';
import { ChatScreen } from './components/chat';
import { SettingsScreen } from './components/settings';
import { NewScreen } from './components/new';
import { JoinScreen } from './components/join';
import { NewDmScreen } from './components/new-dm';
import { PatpNoSig } from '~/types/noun';
import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import GlobalSubscription from '~/logic/subscription/global';
import {groupBunts} from '~/types/group-update';

type ChatAppProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
};

export default class ChatApp extends React.Component<ChatAppProps, {}> {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
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
        stat in associations.chat
      ) {
        totalUnreads += unread;
      }
    });

    const {
      invites,
      s3,
      sidebarShown,
      inbox,
      contacts,
      chatSynced,
      api,
      chatInitialized,
      pendingMessages,
      groups,
      hideAvatars,
      hideNicknames,
      remoteContentPolicy
    } = props;

    const renderChannelSidebar = (props, station?) => (
      <Sidebar
        inbox={inbox}
        messagePreviews={messagePreviews}
        associations={associations}
        contacts={contacts}
        invites={invites['/chat'] || {}}
        unreads={unreads}
        api={api}
        station={station}
        {...props}
      />
    );

    return (
      <>
        <Helmet defer={false}>
          <title>{totalUnreads > 0 ? `(${totalUnreads}) ` : ''}OS1 - Chat</title>
        </Helmet>
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
            path="/~chat/new/dm/:ship?"
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
                    groups={groups || {}}
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
                    groups={groups}
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
            path="/~chat/join/:ship?/:station?"
            render={(props) => {
              let station = `/${props.match.params.ship}/${props.match.params.station}`;

              // ensure we know joined chats
              if(!chatInitialized) {
                return null;
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
                  station={station}
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

            const group = groups[association['group-path']] || groupBunts.group();

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
                  mailboxSize={mailbox.config.length}
                  envelopes={mailbox.envelopes}
                  inbox={inbox}
                  contacts={roomContacts}
                  group={group}
                  pendingMessages={pendingMessages}
                  s3={s3}
                  popout={popout}
                  sidebarShown={sidebarShown}
                  chatInitialized={chatInitialized}
                  hideAvatars={hideAvatars}
                  hideNicknames={hideNicknames}
                  remoteContentPolicy={remoteContentPolicy}
                  {...props}
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
              const popout = props.match.url.includes('/popout/');

              const association =
                station in associations['chat'] ? associations.chat[station] : {};
              const group = groups[association['group-path']] || groupBunts.group();

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
                    groups={groups || {}}
                    group={group}
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
      </>
    );
  }
}
