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
import { MessageScreen } from '/components/lib/message-screen';
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

    this.totalUnseen = 0;
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    //preload spinner asset
    new Image().src = "/~link/img/Spinner.png";
  }

  render() {
    const { state } = this;

    let contacts = !!state.contacts ? state.contacts : {};
    const groups = !!state.groups ? state.groups : {};

    const associations = !!state.associations ? state.associations : {link: {}, contacts: {}};
    let links = !!state.links ? state.links : {};
    let comments = !!state.comments ? state.comments : {};


    const seen = !!state.seen ? state.seen : {};



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

    let selectedGroups = !!state.selectedGroups ? state.selectedGroups : [];

    return (
      <BrowserRouter><Switch>
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
                listening={state.listening}>
                <MessageScreen text="Select or create a collection to begin."/>
              </Skeleton>
            );
          }} />
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
                listening={state.listening}>
                <NewScreen
                  associations={associations}
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
            api.joinCollection(resourcePath);
            props.history.push(makeRoutePath(resourcePath));
          }}
        />
        <Route exact path="/~link/(popout)?/:resource/members"
          render={(props) => {
            const popout = props.match.url.includes("/popout/");
            const resourcePath = '/' + props.match.params.resource;
            const resource = associations.link[resourcePath] || {metadata: {}};

            const contactDetails = contacts[resource["group-path"]] || {};
            const group = groups[resource["group-path"]] || new Set([]);
            const amOwner = amOwnerOfGroup(resource["group-path"]);

            return (
              <Skeleton
                associations={associations}
                invites={invites}
                groups={groups}
                selected={resourcePath}
                sidebarShown={state.sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={state.listening}>
                <MemberScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource["group-path"]}
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
            const resource = associations.link[resourcePath] || false;

            const contactDetails = contacts[resource["group-path"]] || {};
            const group = groups[resource["group-path"]] || new Set([]);
            const amOwner = amOwnerOfGroup(resource["group-path"]);

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
                listening={state.listening}>
                <SettingsScreen
                  sidebarShown={state.sidebarShown}
                  resource={resource}
                  contacts={contacts}
                  contactDetails={contactDetails}
                  groupPath={resource["group-path"]}
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
              const resource = associations.link[resourcePath] || {metadata: {}};

              const amOwner = amOwnerOfGroup(resource["group-path"]);

              let contactDetails = contacts[resource["group-path"]] || {};

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
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={state.listening}>
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
              const resource = associations.link[resourcePath] || {metadata: {}};

              const amOwner = amOwnerOfGroup(resource["group-path"]);

              let popout = props.match.url.includes("/popout/");

              let contactDetails = contacts[resource["group-path"]] || {};

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
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={state.listening}>
                  <LinkDetail
                  {...props}
                  resource={resource}
                  page={page}
                  url={url}
                  linkIndex={index}
                  contacts={contactDetails}
                  resourcePath={resourcePath}
                  groupPath={resource["group-path"]}
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