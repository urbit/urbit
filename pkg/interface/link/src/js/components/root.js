import React, { Component } from 'react';
import { BrowserRouter, Switch, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/new';
import { MemberScreen } from '/components/member';
import { SettingsScreen } from '/components/settings';
import { Links } from '/components/links-list';
import { LinkDetail } from '/components/link';
import { makeRoutePath, amOwnerOfGroup, base64urlDecode } from '../lib/util';

//NOTE route paths make the assumption that a resource identifier is always
//     just a single /path element. technically, backend supports /longer/paths
//     but no tlon-sanctioned frontend creates those right now, so we're opting
//     out of supporting them completely for the time being.

export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    const { state } = this;

    let contacts = !!state.contacts ? state.contacts : {};
    const groups = !!state.groups ? state.groups : {};

    const resources = !!state.resources ? state.resources : {};
    let links = !!state.links ? state.links : {};
    let comments = !!state.comments ? state.comments : {};
    const seen = !!state.seen ? state.seen : {};

    const invites = '/link' in state.invites ?
      state.invites['/link'] : {};

    return (
      <BrowserRouter><Switch>
        <Route exact path="/~link"
          render={ (props) => {
            return (
              <Skeleton
                active="collections"
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                rightPanelHide={true}
                sidebarShown={state.sidebarShown}
                links={links}>
                <div className="h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d dn db-ns">
                <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                      <p className="f8 pt3 gray2 w-100 h-100 dtc v-mid tc">
                        Select or create a collection to begin.
                      </p>
                    </div>
                </div>
              </Skeleton>
            );
          }} />
        <Route exact path="/~link/new"
          render={(props) => {
            return (
              <Skeleton
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                rightPanelHide={true}
                sidebarShown={state.sidebarShown}
                links={links}>
                <NewScreen
                  resources={resources}
                  groups={groups}
                  contacts={contacts}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~link/join/:resource"
          render={ (props) => {
            const resourcePath = '/' + props.match.params.resource;
            props.history.push(makeRoutePath(resourcePath));
          }}
        />
        <Route exact path="/~link/(popout)?/:resource/members"
          render={(props) => {
            const popout = props.match.url.includes("/popout/");
            const resourcePath = '/' + props.match.params.resource;
            const resource = resources[resourcePath] || {};

            const contactDetails = contacts[resource.group] || {};
            const group = groups[resource.group] || new Set([]);
            const amOwner = amOwnerOfGroup(resource.group);

            return (
              <Skeleton
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                selected={resourcePath}
                rightPanelHide={true}
                sidebarShown={state.sidebarShown}
                links={links}>
                <MemberScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource.group}
                  group={group}
                  amOwner={amOwner}
                  resourcePath={resourcePath}
                  popout={popout}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route exact path="/~link/(popout)?/:resource/settings"
          render={ (props) => {
            const popout = props.match.url.includes("/popout/");
            const resourcePath = '/' + props.match.params.resource;
            const resource = resources[resourcePath] || false;

            const contactDetails = contacts[resource.group] || {};
            const group = groups[resource.group] || new Set([]);
            const amOwner = amOwnerOfGroup(resource.group);

            return (
              <Skeleton
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                selected={resourcePath}
                rightPanelHide={true}
                sidebarShown={state.sidebarShown}
                links={links}>
                <SettingsScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource.group}
                  group={group}
                  amOwner={amOwner}
                  resourcePath={resourcePath}
                  popout={popout}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
          <Route exact path="/~link/(popout)?/:resource/:page?"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = resources[resourcePath] || {};

              const amOwner = amOwnerOfGroup(resource.group);

              let contactDetails = contacts[resource.group] || {};

              let page = props.match.params.page || 0;

              let popout = props.match.url.includes("/popout/");

              let channelLinks = !!links[resourcePath]
              ? links[resourcePath]
              : {local: {}};

              let channelComments = !!comments[resourcePath]
                ? comments[resourcePath]
                : {};

              const channelSeen = !!seen[resourcePath]
                ? seen[resourcePath]
                : {};

              return (
                <Skeleton
                  spinner={state.spinner}
                  resources={resources}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}>
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
                  />
                </Skeleton>
              )
            }}
          />
          <Route exact path="/~link/(popout)?/:resource/:page/:index/:encodedUrl/:commentpage?"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = resources[resourcePath] || {};

              const amOwner = amOwnerOfGroup(resource.group);

              let popout = props.match.url.includes("/popout/");

              let contactDetails = contacts[resource.group] || {};

              let index = props.match.params.index || 0;
              let page = props.match.params.page || 0;
              let url = base64urlDecode(props.match.params.encodedUrl);

              let data = !!links[resourcePath]
                ? !!links[resourcePath][page]
                  ? links[resourcePath][page][index]
                  : {}
                : {};
              let coms = !comments[resourcePath]
                ? undefined
                : comments[resourcePath][url];

              let commentPage = props.match.params.commentpage || 0;

              return (
                <Skeleton
                  spinner={state.spinner}
                  resources={resources}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}>
                  <LinkDetail
                  {...props}
                  resource={resource}
                  page={page}
                  url={url}
                  linkIndex={index}
                  contacts={contactDetails}
                  resourcePath={resourcePath}
                  groupPath={resource.group}
                  amOwner={amOwner}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  data={data}
                  comments={coms}
                  commentPage={commentPage}
                  />
                </Skeleton>
              )
            }}
          />
      </Switch></BrowserRouter>
    )
  }
}