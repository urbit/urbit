import React, { Component } from 'react';
import { Switch, Route } from 'react-router-dom';

import _ from 'lodash';

import Api from './api';
import Store from './store';
import Subscription from './subscription';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/new';
import { MemberScreen } from './components/member';
import { SettingsScreen } from './components/settings';
import { MessageScreen } from './components/lib/message-screen';
import { Links } from './components/links-list';
import { LinkDetail } from './components/link';
import { makeRoutePath, amOwnerOfGroup, base64urlDecode } from '../../lib/util';

export class LinksApp extends Component {
  constructor(props) {
    super(props);
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));

    this.state = this.store.state;
    this.totalUnseen = 0;
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
    const groups = state.groups ? state.groups : {};

    const associations = state.associations ? state.associations : { link: {}, contacts: {} };
    const links = state.links ? state.links : {};
    const comments = state.comments ? state.comments : {};

    const seen = state.seen ? state.seen : {};

    const totalUnseen = _.reduce(
      seen,
      (acc, links) => acc + _.reduce(links, (total, hasSeen) => total + (hasSeen ? 0 : 1), 0),
      0
    );

    if(totalUnseen !== this.totalUnseen) {
      document.title = totalUnseen !== 0 ? `Links - (${totalUnseen})` : 'Links';
      this.totalUnseen = totalUnseen;
    }

    const invites = state.invites ?
      state.invites : {};

    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

    return (
      <Switch>
        <Route exact path="/~link"
          render={ (props) => {
            return (
              <Skeleton
                active="collections"
                associations={associations}
                invites={invites}
                groups={groups}
                rightPanelHide={true}
                sidebarShown={state.sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={state.listening}
                api={this.api}
              >
                <MessageScreen text="Select or create a collection to begin." />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~link/new"
          render={(props) => {
            return (
              <Skeleton
                associations={associations}
                invites={invites}
                groups={groups}
                sidebarShown={state.sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={state.listening}
                api={this.api}
              >
                <NewScreen
                  associations={associations}
                  groups={groups}
                  contacts={contacts}
                  api={this.api}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~link/join/:resource"
          render={ (props) => {
            const resourcePath = '/' + props.match.params.resource;
            this.api.joinCollection(resourcePath);
            props.history.push(makeRoutePath(resourcePath));
          }}
        />
        <Route exact path="/~link/(popout)?/:resource/members"
          render={(props) => {
            const popout = props.match.url.includes('/popout/');
            const resourcePath = '/' + props.match.params.resource;
            const resource = associations.link[resourcePath] || { metadata: {} };

            const contactDetails = contacts[resource['group-path']] || {};
            const group = groups[resource['group-path']] || new Set([]);
            const amOwner = amOwnerOfGroup(resource['group-path']);

            return (
              <Skeleton
                associations={associations}
                invites={invites}
                groups={groups}
                selected={resourcePath}
                sidebarShown={state.sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={state.listening}
                api={this.api}
              >
                <MemberScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource['group-path']}
                  group={group}
                  amOwner={amOwner}
                  resourcePath={resourcePath}
                  popout={popout}
                  api={this.api}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~link/(popout)?/:resource/settings"
          render={ (props) => {
            const popout = props.match.url.includes('/popout/');
            const resourcePath = '/' + props.match.params.resource;
            const resource = associations.link[resourcePath] || false;

            const contactDetails = contacts[resource['group-path']] || {};
            const group = groups[resource['group-path']] || new Set([]);
            const amOwner = amOwnerOfGroup(resource['group-path']);

            return (
              <Skeleton
                associations={associations}
                invites={invites}
                groups={groups}
                selected={resourcePath}
                sidebarShown={state.sidebarShown}
                selectedGroups={selectedGroups}
                popout={popout}
                links={links}
                listening={state.listening}
                api={this.api}
              >
                <SettingsScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource['group-path']}
                  group={group}
                  amOwner={amOwner}
                  resourcePath={resourcePath}
                  popout={popout}
                  api={this.api}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
          <Route exact path="/~link/(popout)?/:resource/:page?"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = associations.link[resourcePath] || { metadata: {} };

              const amOwner = amOwnerOfGroup(resource['group-path']);

              const contactDetails = contacts[resource['group-path']] || {};

              const page = props.match.params.page || 0;

              const popout = props.match.url.includes('/popout/');

              const channelLinks = links[resourcePath]
              ? links[resourcePath]
              : { local: {} };

              const channelComments = comments[resourcePath]
                ? comments[resourcePath]
                : {};

              const channelSeen = seen[resourcePath]
                ? seen[resourcePath]
                : {};

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={state.listening}
                  api={this.api}
                >
                  <Links
                  {...props}
                  contacts={contactDetails}
                  links={channelLinks}
                  comments={channelComments}
                  seen={channelSeen}
                  page={page}
                  resourcePath={resourcePath}
                  resource={resource}
                  amOwner={amOwner}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  api={this.api}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~link/(popout)?/:resource/:page/:index/:encodedUrl/:commentpage?"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = associations.link[resourcePath] || { metadata: {} };

              const amOwner = amOwnerOfGroup(resource['group-path']);

              const popout = props.match.url.includes('/popout/');

              const contactDetails = contacts[resource['group-path']] || {};

              const index = props.match.params.index || 0;
              const page = props.match.params.page || 0;
              const url = base64urlDecode(props.match.params.encodedUrl);

              const data = links[resourcePath]
                ? links[resourcePath][page]
                  ? links[resourcePath][page][index]
                  : {}
                : {};
              const coms = !comments[resourcePath]
                ? undefined
                : comments[resourcePath][url];

              const commentPage = props.match.params.commentpage || 0;

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={state.listening}
                  api={this.api}
                >
                  <LinkDetail
                  {...props}
                  resource={resource}
                  page={page}
                  url={url}
                  linkIndex={index}
                  contacts={contactDetails}
                  resourcePath={resourcePath}
                  groupPath={resource['group-path']}
                  amOwner={amOwner}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  data={data}
                  comments={coms}
                  commentPage={commentPage}
                  api={this.api}
                  />
                </Skeleton>
              );
            }}
          />
      </Switch>
    );
  }
}

export default LinksApp;
