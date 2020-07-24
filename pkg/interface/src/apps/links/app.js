import React, { Component } from 'react';
import { Switch, Route } from 'react-router-dom';

import _ from 'lodash';

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
    this.totalUnseen = 0;
  }

  componentDidMount() {
    document.title = 'OS1 - Links';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.api.links.getPage('', 0);
    this.props.subscription.startApp('link');
    if (!this.props.sidebarShown) {
      this.props.api.local.sidebarToggle();
    }
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('link');
  }


  render() {
    const { props } = this;

    const contacts = props.contacts ? props.contacts : {}; 

    const groups = props.groups ? props.groups : {};

    const associations = props.associations ? props.associations : { link: {}, contacts: {} };
    const links = props.links ? props.links : {};
    const comments = props.linkComments ? props.linkComments : {};

    const seen = props.linksSeen ? props.linksSeen : {};

    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

    const selGroupPaths = selectedGroups.map(g => g[0]);
    const totalUnseen = _.reduce(
      links,
      (acc, collection, path) => {
        if(selGroupPaths.length > 0
           && !selGroupPaths.includes(associations.link?.[path]?.['group-path'])) {
          return acc;
        }
        return acc + collection.unseenCount;
      },
      0
    );

    if(totalUnseen !== this.totalUnseen) {
      document.title = totalUnseen !== 0 ? `(${totalUnseen}) OS1 - Links` : 'OS1 - Links';
      this.totalUnseen = totalUnseen;
    }

    const invites = props.invites ?
      props.invites : {};


    const listening = props.linkListening;

    const { api, sidebarShown } = this.props;

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
                sidebarShown={sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={listening}
                api={api}
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
                sidebarShown={sidebarShown}
                selectedGroups={selectedGroups}
                links={links}
                listening={listening}
                api={api}
              >
                <NewScreen
                  associations={associations}
                  groups={groups}
                  contacts={contacts}
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
                selectedGroups={selectedGroups}
                links={links}
                listening={listening}
                api={api}
              >
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
                sidebarShown={sidebarShown}
                selectedGroups={selectedGroups}
                popout={popout}
                links={links}
                listening={listening}
                api={api}
              >
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
                  sidebarShown={sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={listening}
                  api={api}
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
                  sidebarShown={sidebarShown}
                  api={api}
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
                  sidebarShown={sidebarShown}
                  selectedGroups={selectedGroups}
                  sidebarHideMobile={true}
                  popout={popout}
                  links={links}
                  listening={listening}
                  api={api}
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
                  sidebarShown={sidebarShown}
                  data={data}
                  comments={coms}
                  commentPage={commentPage}
                  api={api}
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
