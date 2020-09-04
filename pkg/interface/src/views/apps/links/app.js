import React, { Component } from 'react';
import { Switch, Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import _ from 'lodash';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/new';
import { SettingsScreen } from './components/settings';
import { MessageScreen } from './components/lib/message-screen';
import { Links } from './components/links-list';
import { LinkDetail } from './components/link';

import {
  makeRoutePath,
  amOwnerOfGroup,
  base64urlDecode
} from '~/logic/lib/util';

export class LinksApp extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('graph');
    if (!this.props.sidebarShown) {
      this.props.api.local.sidebarToggle();
    }
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('graph');
  }

  render() {
    const { props } = this;
    const contacts = props.contacts ? props.contacts : {};
    const groups = props.groups ? props.groups : {};
    const associations = props.associations ? props.associations : { link: {}, contacts: {} };
    const graphKeys = props.graphKeys || new Set([]);

    const invites = props.invites ?
      props.invites : {};

    const { api, sidebarShown, hideAvatars, hideNicknames } = this.props;

    return (
      <>
        <Helmet defer={false}>
          <title>OS1 - Links</title>
        </Helmet>
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
                  sidebarShown={sidebarShown}
                  api={api}
                  graphKeys={graphKeys}>
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
                  sidebarShown={sidebarShown}
                  api={api}
                  graphKeys={graphKeys}>
                  <NewScreen
                    api={api}
                    {...props}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~link/join/:resource"
            render={ (props) => {
              const resourcePath = '/' + props.match.params.resource;

              const autoJoin = () => {
                try {
                  api.links.joinCollection(resourcePath);
                  props.history.push(makeRoutePath(resourcePath));
                } catch(err) {
                  setTimeout(autoJoin, 2000);
                }
              };
              autoJoin();
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
                  sidebarShown={sidebarShown}
                  api={api}>
                  <MemberScreen
                    sidebarShown={sidebarShown}
                    resource={resource}
                    contacts={contacts}
                    contactDetails={contactDetails}
                    groupPath={resource['group-path']}
                    group={group}
                    groups={groups}
                    associations={associations}
                    amOwner={amOwner}
                    resourcePath={resourcePath}
                    popout={popout}
                    api={api}
                    {...props} />
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
                  sidebarShown={sidebarShown}
                  popout={popout}
                  api={api}>
                  <SettingsScreen
                    sidebarShown={sidebarShown}
                    resource={resource}
                    contacts={contacts}
                    contactDetails={contactDetails}
                    groupPath={resource['group-path']}
                    group={group}
                    amOwner={amOwner}
                    resourcePath={resourcePath}
                    popout={popout}
                    api={api}
                    {...props} />
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

                return (
                  <Skeleton
                    associations={associations}
                    invites={invites}
                    groups={groups}
                    selected={resourcePath}
                    sidebarShown={sidebarShown}
                    sidebarHideMobile={true}
                    popout={popout}
                    api={api}>
                    <Links
                      {...props}
                      contacts={contactDetails}
                      page={page}
                      resourcePath={resourcePath}
                      resource={resource}
                      amOwner={amOwner}
                      popout={popout}
                      sidebarShown={sidebarShown}
                      api={api}
                      hideNicknames={hideNicknames}
                      hideAvatars={hideAvatars} />
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

                return (
                  <Skeleton
                    associations={associations}
                    invites={invites}
                    groups={groups}
                    selected={resourcePath}
                    sidebarShown={sidebarShown}
                    sidebarHideMobile={true}
                    popout={popout}
                    api={api}>
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
                      sidebarShown={sidebarShown}
                      api={api}
                      hideAvatars={hideAvatars}
                      hideNicknames={hideNicknames} />
                  </Skeleton>
                );
              }}
            />
        </Switch>
      </>
    );
  }
}

export default LinksApp;
