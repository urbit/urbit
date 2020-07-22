import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/new';
import { ContactSidebar } from './components/lib/contact-sidebar';
import { ContactCard } from './components/lib/contact-card';
import { AddScreen } from './components/lib/add-contact';
import { JoinScreen } from './components/join';
import GroupDetail from './components/lib/group-detail';

import { PatpNoSig } from '../../types/noun';
import GlobalApi from '../../api/global';
import { StoreState } from '../../store/type';
import GlobalSubscription from '../../subscription/global';


type GroupsAppProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
}

export default class GroupsApp extends Component<GroupsAppProps, {}> {
  componentDidMount() {
    document.title = 'OS1 - Groups';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('groups')
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('groups')
  }


  render() {
    const { props } = this;

    const contacts = props.contacts || {};
    const defaultContacts =
      (Boolean(props.contacts) && '/~/default' in props.contacts) ?
        props.contacts['/~/default'] : {};

    const invites =
      (Boolean(props.invites) && '/contacts' in props.invites) ?
        props.invites['/contacts'] : {};
    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];
    const s3 = props.s3 ? props.s3 : {};
    const groups = props.groups || {};
    const associations = props.associations || {};
    const { api } = props;


    return (
        <Switch>
          <Route exact path="/~groups"
            render={(props) => {
              return (
                <Skeleton
                  activeDrawer="groups"
                  selectedGroups={selectedGroups}
                  history={props.history}
                  api={api}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  associations={associations}
                >
                  <div className="h-100 w-100 overflow-x-hidden bg-white bg-gray0-d dn db-ns">
                    <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                      <p className="f9 pt3 gray2 w-100 h-100 dtc v-mid tc">
                        Select a group to begin.
                    </p>
                    </div>
                  </div>
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/new"
            render={(props) => {
              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={api}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  associations={associations}
                  activeDrawer="rightPanel"
                >
                  <NewScreen
                    history={props.history}
                    groups={groups}
                    contacts={contacts}
                    api={api}
                  />
                </Skeleton>
              );
            }}
          />
        <Route exact path="/~groups/join/:ship?/:name?"
            render={(props) => {
              const ship = props.match.params.ship || '';
              const name = props.match.params.name || '';
              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={api}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  associations={associations}
                  activeDrawer="rightPanel"
                >
                  <JoinScreen
                    history={props.history}
                    groups={groups}
                    contacts={contacts}
                    api={api}
                    ship={ship}
                    name={name}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/(detail)?/(settings)?/ship/:ship/:group/"
            render={(props) => {
              const groupPath =
                `/ship/${props.match.params.ship}/${props.match.params.group}`;
              const groupContacts = contacts[groupPath] || {};
              const group = groups[groupPath] ;
              const detail = Boolean(props.match.url.includes('/detail'));
              const settings = Boolean(props.match.url.includes('/settings'));

              const association = (associations.contacts?.[groupPath])
                ? associations.contacts[groupPath]
                : {};
              if(!group) {
                return null;
              }

              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={api}
                  contacts={contacts}
                  invites={invites}
                  groups={groups}
                  activeDrawer={(detail || settings) ? 'detail' : 'contacts'}
                  selected={groupPath}
                  associations={associations}
                >
                  <ContactSidebar
                    contacts={groupContacts}
                    defaultContacts={defaultContacts}
                    group={group}
                    activeDrawer={(detail || settings) ? 'detail' : 'contacts'}
                    api={api}
                    path={groupPath}
                    groups={groups}
                    {...props}
                  />
                  <GroupDetail
                    association={association}
                    path={groupPath}
                    group={group}
                    groups={groups}
                    activeDrawer={(detail || settings) ? 'detail' : 'contacts'}
                    settings={settings}
                    associations={associations}
                    api={api}
                    {...props}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/add/ship/:ship/:group"
            render={(props) => {
              const groupPath =
                `/ship/${props.match.params.ship}/${props.match.params.group}`;
              const groupContacts = contacts[groupPath] || {};
              const group = groups[groupPath] || {};

              if(!group) {
                return null;
              }

              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={api}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  activeDrawer="rightPanel"
                  selected={groupPath}
                  associations={associations}
                >
                  <ContactSidebar
                    contacts={groupContacts}
                    defaultContacts={defaultContacts}
		    group={group}
		    groups={groups}
                    activeDrawer="rightPanel"
                    path={groupPath}
                    api={api}
                    {...props}
                  />
                  <AddScreen
                    api={api}
                    groups={groups}
                    path={groupPath}
                    history={props.history}
                    contacts={contacts}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/share/ship/:ship/:group"
            render={(props) => {
              const groupPath =
                `/ship/${props.match.params.ship}/${props.match.params.group}`;
              const shipPath = `${groupPath}/${window.ship}`;
              const rootIdentity = defaultContacts[window.ship] || {};

              const groupContacts = contacts[groupPath] || {};
              const contact =
                (window.ship in groupContacts) ?
                  groupContacts[window.ship] : {};
              const group = groups[groupPath];
              if(!group) {
                return null;
              }

              return (
                <Skeleton
                  history={props.history}
                  api={api}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  activeDrawer="rightPanel"
                  selected={groupPath}
                  associations={associations}
                >
                  <ContactSidebar
                    activeDrawer="rightPanel"
                    contacts={groupContacts}
                    defaultContacts={defaultContacts}
                    group={group}
                    path={groupPath}
                    api={api}
                    selectedContact={shipPath}
                    groups={groups}
                    {...props}
                  />
                  <ContactCard
                    api={api}
                    history={props.history}
                    contact={contact}
                    path={groupPath}
                    ship={window.ship}
                    share={true}
                    rootIdentity={rootIdentity}
                    s3={s3}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/view/ship/:ship/:group/:contact"
            render={(props) => {
              const groupPath =
                `/ship/${props.match.params.ship}/${props.match.params.group}`;
              const shipPath =
                `${groupPath}/${props.match.params.contact}`;

              const groupContacts = contacts[groupPath] || {};
              const contact =
                (props.match.params.contact in groupContacts) ?
                  groupContacts[props.match.params.contact] : {};
              const group = groups[groupPath] ;

              const rootIdentity =
                props.match.params.contact === window.ship ?
                defaultContacts[window.ship] : null;
              if(!group) {
                return null;
              }

              return (
                <Skeleton
                  history={props.history}
                  api={api}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  activeDrawer="rightPanel"
                  selected={groupPath}
                  associations={associations}
                >
                  <ContactSidebar
                    activeDrawer="rightPanel"
                    contacts={groupContacts}
                    defaultContacts={defaultContacts}
                    group={group}
                    path={groupPath}
                    api={api}
                    selectedContact={shipPath}
                    groups={groups}
                    {...props}
                  />
                  <ContactCard
                    api={api}
                    history={props.history}
                    contact={contact}
                    path={groupPath}
                    ship={props.match.params.contact}
                    rootIdentity={rootIdentity}
                    s3={s3}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/me"
            render={(props) => {
              const me = defaultContacts[window.ship] || {};

              return (
                <Skeleton
                  history={props.history}
                  api={api}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  activeDrawer="rightPanel"
                  selected="me"
                  associations={associations}
                >
                  <ContactCard
                    api={api}
                    history={props.history}
                    path="/~/default"
                    contact={me}
                    s3={s3}
                    ship={window.ship}
                  />
                </Skeleton>
              );
            }}
          />
        </Switch>
    );
  }
}

