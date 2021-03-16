import React, { Component, useEffect, useCallback, ReactElement } from 'react';
import { Route, Switch, RouteComponentProps } from 'react-router-dom';
import Helmet from 'react-helmet';

import './css/custom.css';

import { graph, PatpNoSig } from '@urbit/api';
import { StoreState } from '~/logic/store/type';
import { GroupsPane } from './components/GroupsPane';
import { NewGroup } from './components/NewGroup';
import { JoinGroup } from './components/JoinGroup';

import { cite } from '~/logic/lib/util';
import { Body } from '../components/Body';
import { Box } from '@tlon/indigo-react';
import { Loading } from '../components/Loading';
import { Workspace } from '~/types/workspace';
import useGraphState from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import withState from '~/logic/lib/withState';
import { createUnmanagedGraph } from '@urbit/api/graph';
import useApi from '~/logic/api';

type LandscapeProps = StoreState & {
  ship: PatpNoSig;
}

export function DMRedirect(props: LandscapeProps & RouteComponentProps & { ship: string; }): ReactElement {
  const { ship, history } = props;
  const api = useApi();
  const goToGraph = useCallback((graph: string) => {
    history.push(`/~landscape/messages/resource/chat/ship/~${graph}`);
  }, [history]);
  const graphKeys = useGraphState(state => state.graphKeys);

  useEffect(() => {
    const station = `${window.ship}/dm--${ship}`;
    const theirStation = `${ship}/dm--${window.ship}`;

    if (graphKeys.has(station)) {
      goToGraph(station);
      return;
    }

    if (graphKeys.has(theirStation)) {
      goToGraph(theirStation);
      return;
    }

    const aud = ship !== window.ship ? [`~${ship}`] : [];
    const title = `${cite(window.ship)} <-> ${cite(ship)}`;

    api.thread(graph.createUnmanagedGraph(
      window.ship,
      `dm--${ship}`,
      title,
      '',
      { invite: { pending: aud } },
      'chat'
    )).then(() => {
      goToGraph(station);
    });
  }, []);

  return (
    <Loading text="Creating DM" />
  );
}

const Landscape = (props: LandscapeProps) => {
  const notificationsCount = useHarkState(state => state.notificationsCount)
  return (
    <>
      <Helmet defer={false}>
        <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Landscape</title>
      </Helmet>
      <Switch>
        <Route path="/~landscape/ship/:host/:name"
          render={(routeProps) => {
            const {
              host,
              name
            } = routeProps.match.params as Record<string, string>;
            const groupPath = `/ship/${host}/${name}`;
            const baseUrl = `/~landscape${groupPath}`;
            const ws: Workspace = { type: 'group', group: groupPath };

            return (
              <GroupsPane workspace={ws} baseUrl={baseUrl} {...props} />
            );
          }}
        />
        <Route path="/~landscape/home"
          render={() => {
            const ws: Workspace = { type: 'home' };
            return (
              <GroupsPane workspace={ws} baseUrl="/~landscape/home" {...props} />
            );
          }}
        />
        <Route path="/~landscape/messages"
          render={() => {
            const ws: Workspace = { type: 'messages' };
            return (
              <GroupsPane workspace={ws} baseUrl="/~landscape/messages" {...props} />
            );
          }}
        />
        <Route path="/~landscape/new"
          render={(routeProps) => {
            return (
              <Body>
                <Box maxWidth="300px">
                  <NewGroup
                    {...routeProps}
                  />
                </Box>
              </Body>
            );
          }}
        />
        <Route path='/~landscape/dm/:ship?'
        render={(routeProps) => {
          const { ship } = routeProps.match.params;
          return <DMRedirect {...routeProps} {...props} ship={ship} />;
        }}
        />
        <Route path="/~landscape/join/:ship?/:name?"
          render={(routeProps) => {
            const { ship, name } = routeProps.match.params;
            const autojoin = ship && name ? `${ship}/${name}` : null;
            return (
              <Body>
                <Box maxWidth="300px">
                  <JoinGroup
                    autojoin={autojoin}
                    {...routeProps}
                  />
                </Box>
              </Body>
            );
          }}
        />
      </Switch>
    </>
  );
}

export default Landscape;