import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom';
import { Box, Center } from '@tlon/indigo-react';

import './css/custom.css';

import { PatpNoSig, AppName } from '~/types/noun';
import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import GlobalSubscription from '~/logic/subscription/global';
import { Resource } from '~/views/components/Resource';
import { PopoverRoutes } from './components/PopoverRoutes';
import { UnjoinedResource } from '~/views/components/UnjoinedResource';
import { GroupsPane } from './components/GroupsPane';
import { Workspace } from '~/types';
import {NewGroup} from './components/NewGroup';
import {JoinGroup} from './components/JoinGroup';


type LandscapeProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
}

export default class Landscape extends Component<LandscapeProps, {}> {
  componentDidMount() {
    document.title = 'OS1 - Landscape';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('groups')
    this.props.subscription.startApp('chat')
    this.props.subscription.startApp('publish');
    this.props.subscription.startApp('graph');
    this.props.api.publish.fetchNotebooks();
    
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
    const s3 = props.s3 ? props.s3 : {};
    const groups = props.groups || {};
    const associations = props.associations || {};
    const { api } = props;


    return (
      <Switch>
        <Route path="/~landscape/ship/:host/:name"
          render={routeProps => {
            const {
              host,
              name
            } = routeProps.match.params as Record<string, string>;
            const groupPath = `/ship/${host}/${name}`;
            const baseUrl = `/~landscape${groupPath}`;
            const ws: Workspace = { type: 'group', group: groupPath };

            return (
              <GroupsPane workspace={ws} baseUrl={baseUrl} {...props} />
            )
          }}/>
        <Route path="/~landscape/home"
          render={routeProps => {
            const ws: Workspace = { type: 'home' };

            return (
              <GroupsPane workspace={ws} baseUrl="/~landscape/home" {...props} />
            );
          }}
        />
        <Route path="/~landscape/new"
          render={routeProps=> {
            return (
              <NewGroup
                groups={props.groups}
                contacts={props.contacts}
                api={props.api} 
                {...routeProps}
              />
            );
          }}
        />
        <Route path="/~landscape/join"
          render={routeProps=> {
            return (
              <JoinGroup 
                groups={props.groups}
                contacts={props.contacts}
                api={props.api} 
                {...routeProps}
              />
            );
          }}
        />
      </Switch>
    );
  }
}

