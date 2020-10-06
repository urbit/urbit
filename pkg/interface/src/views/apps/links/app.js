import React, { Component } from 'react';
import { Switch, Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import _ from 'lodash';

import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { NewScreen } from './components/new';
import { SettingsScreen } from './components/settings';
import { MessageScreen } from './components/lib/message-screen';
import { LinkList } from './components/link-list';
import { LinkDetail } from './components/link-detail';

import {
  amOwnerOfGroup,
  base64urlDecode
} from '~/logic/lib/util';


export default class LinksApp extends Component {
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
    const associations =
      props.associations ? props.associations : { graph: {}, contacts: {} };
    const graphKeys = props.graphKeys || new Set([]);
    const graphs = props.graphs || {};

    const invites = props.invites ?
      props.invites : {};

    const {
      api, sidebarShown, s3,
      hideAvatars, hideNicknames, remoteContentPolicy
    } = this.props;

    return (
      <>
        <Helmet defer={false}>
          <title>OS1 - Links</title>
        </Helmet>
        <Switch>
          <Route exact path="/~link"
            render={ (props) => (
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
            )}
          />
          <Route exact path="/~link/new"
            render={ (props) => (
              <Skeleton
                associations={associations}
                invites={invites}
                groups={groups}
                sidebarShown={sidebarShown}
                api={api}
                graphKeys={graphKeys}>
                <NewScreen
                  api={api}
                  graphKeys={graphKeys}
                  associations={associations}
                  groups={groups}
                  {...props}
                />
              </Skeleton>
            )}
          />
          <Route exact path="/~link/:ship/:name/settings"
            render={ (props) => {
              const resourcePath =
                `${props.match.params.ship}/${props.match.params.name}`;
              const metPath = `/ship/~${resourcePath}`;
              const resource =
                associations.graph[metPath] ?
                  associations.graph[metPath] : { metadata: {} };

              const contactDetails = contacts[resource['group-path']] || {};
              const group = groups[resource['group-path']] || new Set([]);
              const amOwner = amOwnerOfGroup(resource['group-path']);
              const hasGraph = !!graphs[resourcePath];

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={sidebarShown}
                  graphKeys={graphKeys}
                  api={api}>
                  <SettingsScreen
                    sidebarShown={sidebarShown}
                    resource={resource}
                    contacts={contacts}
                    contactDetails={contactDetails}
                    graphResource={graphKeys.has(resourcePath)}
                    hasGraph={!!hasGraph}
                    group={group}
                    amOwner={amOwner}
                    resourcePath={resourcePath}
                    api={api}
                    {...props} />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~link/:ship/:name"
            render={ (props) => {
              const resourcePath =
                `${props.match.params.ship}/${props.match.params.name}`;
              const metPath = `/ship/~${resourcePath}`;
              const resource =
                associations.graph[metPath] ?
                  associations.graph[metPath] : { metadata: {} };

              const contactDetails = contacts[resource['group-path']] || {};
              const graph = graphs[resourcePath] || null;

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={sidebarShown}
                  sidebarHideMobile={true}
                  api={api}
                  graphKeys={graphKeys}>
                  <LinkList
                    {...props}
                    api={api}
                    s3={s3}
                    graph={graph}
                    graphResource={graphKeys.has(resourcePath)}
                    resourcePath={resourcePath}
                    metadata={resource.metadata}
                    contacts={contactDetails}
                    hideAvatars={hideAvatars}
                    hideNicknames={hideNicknames}
                    sidebarShown={sidebarShown}
                    ship={props.match.params.ship}
                    name={props.match.params.name}
                  />
                </Skeleton>
              );
            }}
          />
          <Route exact path="/~link/:ship/:name/:index"
            render={ (props) => {
              const resourcePath =
                `${props.match.params.ship}/${props.match.params.name}`;
              const metPath = `/ship/~${resourcePath}`;
              const resource =
                associations.graph[metPath] ?
                  associations.graph[metPath] : { metadata: {} };

              const contactDetails = contacts[resource['group-path']] || {};

              const indexArr = props.match.params.index.split('-');
              const graph = graphs[resourcePath] || null;

              if (indexArr.length <= 1) {
                return <div>Malformed URL</div>;
              }

              const index = parseInt(indexArr[1], 10);
              const node = Boolean(graph) ? graph.get(index) : null;

              return (
                <Skeleton
                  associations={associations}
                  invites={invites}
                  groups={groups}
                  selected={resourcePath}
                  sidebarShown={sidebarShown}
                  sidebarHideMobile={true}
                  graphKeys={graphKeys}
                  api={api}>
                  <LinkDetail
                    {...props}
                    node={node}
                    graphResource={graphKeys.has(resourcePath)}
                    ship={props.match.params.ship}
                    name={props.match.params.name}
                    resource={resource}
                    contacts={contactDetails}
                    sidebarShown={sidebarShown}
                    api={api}
                    hideAvatars={hideAvatars}
                    hideNicknames={hideNicknames}
                    remoteContentPolicy={remoteContentPolicy} />
                </Skeleton>
              );
            }}
          />
        </Switch>
      </>
    );
  }
}

