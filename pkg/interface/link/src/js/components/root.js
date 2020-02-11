import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { Links } from '/components/links-list';
import { LinkDetail } from '/components/link';
import { base64urlDecode } from '../lib/util';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
    this.setSpinner = this.setSpinner.bind(this);
  }

  setSpinner(spinner) {
    this.setState({
      spinner
    });
  }

  render() {
    const { props, state } = this;

    let contacts = !!state.contacts ? state.contacts : {};
    const groups = !!state.groups ? state.groups : {};

    let links = !!state.links ? state.links : {};
    let comments = !!state.comments ? state.comments : {};
    const seen = !!state.seen ? state.seen : {};

    return (
      <BrowserRouter>
        <Route exact path="/~link"
          render={ (props) => {
            return (
              <Skeleton
                active="channels"
                groups={groups}
                rightPanelHide={true}
                sidebarShown={true}
                links={links}>
                <div className="h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d dn db-ns">
                <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                      <p className="f8 pt3 gray2 w-100 h-100 dtc v-mid tc">
                        Collections are shared across groups. To create a new collection, <a className="black white-d" href="/~contacts">create a group</a>.
                      </p>
                    </div>
                </div>
              </Skeleton>
            );
          }} />
          <Route exact path="/~link/(popout)?/:ship/:channel/:page?"
            render={ (props) => {
              // groups/contacts and link channels are the same thing in ver 1

              let groupPath =
              `/${props.match.params.ship}/${props.match.params.channel}`;
              let contactDetails = contacts[groupPath] || {};

              let page = props.match.params.page || 0;

              let popout = props.match.url.includes("/popout/");

              let channelLinks = !!links[groupPath]
              ? links[groupPath]
              : {local: {}};

              let channelComments = !!comments[groupPath]
                ? comments[groupPath]
                : {};

              const channelSeen = !!seen[groupPath]
                ? seen[groupPath]
                : {};

              return (
                <Skeleton
                  spinner={state.spinner}
                  groups={groups}
                  active="links"
                  selected={groupPath}
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
                  groupPath={groupPath}
                  popout={popout}
                  sidebarShown={state.sidebarShown}
                  />
                </Skeleton>
              )
            }}
          />
          <Route exact path="/~link/(popout)?/:ship/:channel/:page/:index/:encodedUrl/(comments)?/:commentpage?"
            render={ (props) => {
              let groupPath =
              `/${props.match.params.ship}/${props.match.params.channel}`;

              let popout = props.match.url.includes("/popout/");

              let contactDetails = contacts[groupPath] || {};

              let index = props.match.params.index || 0;
              let page = props.match.params.page || 0;
              let url = base64urlDecode(props.match.params.encodedUrl);

              let data = !!links[groupPath]
                ? !!links[groupPath][page]
                  ? links[groupPath][page][index]
                  : {}
                : {};
              let coms = !comments[groupPath]
                ? undefined
                : comments[groupPath][url];

              let commentPage = props.match.params.commentpage || 0;

              return (
                <Skeleton
                  spinner={state.spinner}
                  groups={groups}
                  active="links"
                  selected={groupPath}
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
                  groupPath={groupPath}
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