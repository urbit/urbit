import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { NewScreen } from '/components/new';
import { Links } from '/components/links-list';
import { LinkDetail } from '/components/link';
import { base64urlDecode } from '../lib/util';


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
      <BrowserRouter>
        <Route exact path="/~link"
          render={ (props) => {
            return (
              <Skeleton
                active="channels"
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                rightPanelHide={true}
                sidebarShown={true}
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
                active="channels"
                spinner={state.spinner}
                resources={resources}
                invites={invites}
                groups={groups}
                rightPanelHide={true}
                sidebarShown={true}
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
        <Route exact path="/~link/join/:resource([^#]+)"
          render={ (props) => {
            const resourcePath = '/' + props.match.params.resource;
            props.history.push(`/~link/list/0${resourcePath}`);
          }}
        />
          <Route exact path="/~link/(popout)?/list/:page/:resource([^#]+)"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = resources[resourcePath] || {};

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
                  active="links"
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
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  />
                </Skeleton>
              )
            }}
          />
          <Route exact path="/~link/(popout)?/item/:page/:index/:commentpage/:encodedUrl/:resource([^#]+)"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;
              const resource = resources[resourcePath] || {};

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
                  active="links"
                  selected={resourcePath}
                  sidebarShown={state.sidebarShown}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}>
                  <LinkDetail
                  {...props}
                  page={page}
                  url={url}
                  linkIndex={index}
                  contacts={contactDetails}
                  resourcePath={resourcePath}
                  groupPath={resource.group}
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
      </BrowserRouter>
    )
  }
}