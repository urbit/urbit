import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom';

import './css/custom.css';

import { PatpNoSig } from '~/types/noun';
import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import { GroupsPane } from './components/GroupsPane';
import { Workspace } from '~/types';
import { NewGroup } from './components/NewGroup';
import { JoinGroup } from './components/JoinGroup';

import { cite } from '~/logic/lib/util';
import { Body } from '../components/Body';
import { Box } from '@tlon/indigo-react';


type LandscapeProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
}

export default class Landscape extends Component<LandscapeProps, {}> {
  componentDidMount() {
    document.title = 'OS1 - Landscape';

    this.props.subscription.startApp('groups');
    this.props.subscription.startApp('chat');
    this.props.subscription.startApp('graph');
  }

  createandRedirectToDM(api, ship, history, allStations) {
    const station = `/~${window.ship}/dm--${ship}`;
    const theirStation = `/~${ship}/dm--${window.ship}`;

    if (allStations.indexOf(station) !== -1) {
      history.push(`/~landscape/home/resource/chat${station}`);
      return;
    }

    if (allStations.indexOf(theirStation) !== -1) {
      history.push(`/~landscape/home/resource/chat${theirStation}`);
      return;
    }

    const groupPath = `/ship/~${window.ship}/dm--${ship}`;
    const aud = ship !== window.ship ? [`~${ship}`] : [];
    const title = `${cite(window.ship)} <-> ${cite(ship)}`;

    api.chat.create(
      title,
      '',
      station,
      groupPath,
      { invite: { pending: aud } },
      aud,
      true,
      false
    );

    //  TODO: make a pretty loading state
    setTimeout(() => {
      history.push(`/~landscape/home/resource/chat${station}`);
    }, 5000);
  }

  render() {
    const { props } = this;
    const { api, inbox } = props;

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
              <Body>
                <Box maxWidth="300px">
                  <NewGroup
                    groups={props.groups}
                    contacts={props.contacts}
                    api={props.api}
                    {...routeProps}
                  />
                </Box>
              </Body>
            );
          }}
        />
        <Route path='/~landscape/dm/:ship?'
        render={routeProps => {
          const { ship } = routeProps.match.params;
          return this.createandRedirectToDM(api, ship, routeProps.history, Object.keys(inbox));
        }}
        />
        <Route path="/~landscape/join/:ship?/:name?"
          render={routeProps=> {
            const { ship, name } = routeProps.match.params;
            const autojoin = ship && name ? `${ship}/${name}` : null;
            return (
              <Body>
                <Box maxWidth="300px">
                  <JoinGroup
                    groups={props.groups}
                    contacts={props.contacts}
                    api={props.api}
                    autojoin={autojoin}
                    {...routeProps}
                  />
                </Box>
              </Body>
            );
          }}
        />
      </Switch>
    );
  }
}

