import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom';
import { Box, Center } from '@tlon/indigo-react';

import './css/custom.css';

import { Skeleton as NewSkeleton } from '~/views/components/Skeleton';

import { PatpNoSig, AppName } from '~/types/noun';
import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import GlobalSubscription from '~/logic/subscription/global';
import {Resource} from '~/views/components/Resource';
import {PopoverRoutes} from './components/PopoverRoutes';
import {UnjoinedResource} from '~/views/components/UnjoinedResource';
import {GroupsPane} from '~/views/components/GroupsPane';
import {Workspace} from '~/types';


type GroupsAppProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
}

export default class GroupsApp extends Component<GroupsAppProps, {}> {
  componentDidMount() {
    document.title = 'OS1 - Groups';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.props.subscription.startApp('groups')
    this.props.subscription.startApp('chat')
    this.props.subscription.startApp('publish');
    this.props.subscription.startApp('graph');
    
  }

  componentWillUnmount() {
    this.props.subscription.stopApp('groups')
    this.props.subscription.stopApp('chat')
    this.props.subscription.stopApp('publish');
    this.props.subscription.stopApp('graph');
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
        <Route path="/~groups/ship/:host/:name"
          render={routeProps => {
            const {
              host,
              name
            } = routeProps.match.params as Record<string, string>;
            const groupPath = `/ship/${host}/${name}`;
            const baseUrl = `/~groups${groupPath}`;
            const ws: Workspace = { type: 'group', group: groupPath };

            return (
              <GroupsPane workspace={ws} baseUrl={baseUrl} {...props} />
            )
          }}/>
        <Route path="/~groups/home"
          render={routeProps => {
            const ws: Workspace = { type: 'home' };

            return (
              <GroupsPane workspace={ws} baseUrl="/~groups/home" {...props} />
            );
          }}
        />
      </Switch>
    );
  }
}

