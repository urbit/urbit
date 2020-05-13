import React, { Component } from 'react';
import { Route } from 'react-router-dom';

import Api from './api';
import Subscription from './subscription';
import Store from './store';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/new';
import { ContactSidebar } from './components/lib/contact-sidebar';
import { ContactCard } from './components/lib/contact-card';
import { AddScreen } from './components/lib/add-contact';
import GroupDetail from './components/lib/group-detail';

export default class GroupsApp extends Component {
  constructor(props) {
    super(props);
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));

    this.state = this.store.state;
    this.resetControllers();
  }

  componentDidMount() {
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

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  render() {
    const { state, props } = this;

    const contacts = state.contacts ? state.contacts : {};
    const defaultContacts =
      (Boolean(state.contacts) && '/~/default' in state.contacts) ?
        state.contacts['/~/default'] : {};
    const groups = state.groups ? state.groups : {};

    const invites =
      (Boolean(state.invites) && '/contacts' in state.invites) ?
        state.invites['/contacts'] : {};
    const associations = state.associations ? state.associations : {};
    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];
    const s3 = state.s3 ? state.s3 : {};

    return (
        <div>
          <Route exact path="/~groups"
            render={(props) => {
              return (
                <Skeleton
                  activeDrawer="groups"
                  selectedGroups={selectedGroups}
                  history={props.history}
                  api={this.api}
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
                  api={this.api}
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
                    api={this.api}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/(detail)?/(settings)?/:ship/:group/"
            render={(props) => {
              const groupPath =
                `/${props.match.params.ship}/${props.match.params.group}`;
              const groupContacts = contacts[groupPath] || {};
              const group = groups[groupPath] || new Set([]);
              const detail = Boolean(props.match.url.includes('/detail'));
              const settings = Boolean(props.match.url.includes('/settings'));

              const association = (associations[groupPath])
                ? associations[groupPath]
                : {};

              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={this.api}
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
                    api={this.api}
                    path={groupPath}
                    {...props}
                  />
                  <GroupDetail
                    association={association}
                    path={groupPath}
                    group={group}
                    activeDrawer={(detail || settings) ? 'detail' : 'contacts'}
                    settings={settings}
                    api={this.api}
                    {...props}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/add/:ship/:group"
            render={(props) => {
              const groupPath =
                `/${props.match.params.ship}/${props.match.params.group}`;
              const groupContacts = contacts[groupPath] || {};
              const group = groups[groupPath] || new Set([]);

              return (
                <Skeleton
                  history={props.history}
                  selectedGroups={selectedGroups}
                  api={this.api}
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
                    activeDrawer="rightPanel"
                    path={groupPath}
                    api={this.api}
                    {...props}
                  />
                  <AddScreen
                    api={this.api}
                    groups={groups}
                    path={groupPath}
                    history={props.history}
                    contacts={contacts}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~groups/share/:ship/:group"
            render={(props) => {
              const groupPath =
                `/${props.match.params.ship}/${props.match.params.group}`;
              const shipPath = `${groupPath}/${window.ship}`;
              const rootIdentity = defaultContacts[window.ship] || {};

              const groupContacts = contacts[groupPath] || {};
              const contact =
                (window.ship in groupContacts) ?
                  groupContacts[window.ship] : {};
              const group = groups[groupPath] || new Set([]);

              return (
                <Skeleton
                  history={props.history}
                  api={this.api}
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
                    api={this.api}
                    selectedContact={shipPath}
                    {...props}
                  />
                  <ContactCard
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
          <Route exact path="/~groups/view/:ship/:group/:contact"
            render={(props) => {
              const groupPath =
                `/${props.match.params.ship}/${props.match.params.group}`;
              const shipPath =
                `${groupPath}/${props.match.params.contact}`;

              const groupContacts = contacts[groupPath] || {};
              const contact =
                (props.match.params.contact in groupContacts) ?
                  groupContacts[props.match.params.contact] : {};
              const group = groups[groupPath] || new Set([]);

              const rootIdentity =
                props.match.params.contact === window.ship ?
                  defaultContacts[window.ship] : null;

              return (
                <Skeleton
                  history={props.history}
                  api={this.api}
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
                    api={this.api}
                    selectedContact={shipPath}
                    {...props}
                  />
                  <ContactCard
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
                  api={this.api}
                  selectedGroups={selectedGroups}
                  contacts={contacts}
                  groups={groups}
                  invites={invites}
                  activeDrawer="rightPanel"
                  selected="me"
                  associations={associations}
                >
                  <ContactCard
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
        </div>
    );
  }
}

