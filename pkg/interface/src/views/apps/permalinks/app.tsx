import { Association, deSig, GraphConfig } from '@urbit/api';
import React, { useCallback } from 'react';
import {
  Redirect, Route, Switch
} from 'react-router-dom';
import { useQuery } from '~/logic/lib/useQuery';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import { getGraphPermalink } from './graphIndex';

interface ResourceRouteProps {
  ship: string;
  name: string;
}

export function PermalinkRoutes(props: unknown) {
  const groups = useGroupState(s => s.groups);
  const { query, toQuery } = useQuery();

  return (
    <Switch>
      <Route
        path="/perma/group/:ship/:name"
        render={({ match, history, location }) => {
          const { ship, name } = match.params as ResourceRouteProps;
          const { url } = match;
          const path = `/ship/${ship}/${name}`;
          const group = groups[path];
          if(!group) {
            if (Object.keys(groups).length > 0) {
              const redir = location.pathname;
              const to = toQuery({ redir }, `/~landscape/join/${ship}/${name}`);
              return <Redirect to={to} />;
            }
            return null;
          }

          return <GroupRoutes url={url} group={path} />;
        }}
      />
      <Route path="/perma" render={() => <FallbackRoutes query={query} />} />
    </Switch>
  );
}

function FallbackRoutes(props: { query: URLSearchParams }) {
  const { query } = props;

  if (query.has('ext')) {
    const ext = query.get('ext')!;
    const url = `/perma${ext.slice(16)}`;
    return <Redirect to={{ pathname: url }} />;
  }

  return <Redirect to="/~404" />;
}

function GroupRoutes(props: { group: string; url: string }) {
  const { group, url } = props;
  const makePath = (s: string) => url + s;
  const associations = useMetadataState(s => s.associations);
  const graphKeys = useGraphState(s => s.graphKeys);
  const { toQuery } = useQuery();
  const groupUrl = `/~landscape${group}`;

  return (
    <Switch>
      <Route
        path={makePath('/graph/:ship/:name')}
        render={({ match, location }) => {
          const { ship, name } = match.params as unknown as ResourceRouteProps;
          const path = `/ship/${ship}/${name}`;
          const association = associations.graph[path];
          const { url: routeUrl } = match;
          if(!association) {
            return null;
          }
          if(!graphKeys.has(`${deSig(ship)}/${name}`)) {
            if(graphKeys.size > 1) { // TODO: Better loading logic see https://github.com/urbit/landscape/issues/1063
              return <Redirect
                to={toQuery(
                  { auto: 'y', redir: location.pathname },
                  `${groupUrl}/join/${(association.metadata.config as GraphConfig).graph}${path}`
                )}
                     />;
            }
            return null;
          }

          return <GraphIndexRoutes url={routeUrl} association={association} />;
        }}
      />
      <Route
        exact
        path={makePath('')}
        render={() => {
          return <Redirect to={groupUrl} />;
        }}
      />
    </Switch>
  );
}

export function GraphIndexRoutes(props: {
  association: Association;
  url: string;
  index?: string;
}) {
  const { index = '', association, url } = props;
  const makePath = (s: string) => url + s;
  const group = useGroupState(
    useCallback(s => s.groups[association.group], [association])
  );

  if(!group) {
    return null;
  }

  return (
    <Switch>
      <Route
        path={makePath('/:id')}
        render={({ match }) => {
          const newIndex = `${index}/${match.params.id}`;
          const { url: newUrl } = match;
          return (
            <GraphIndexRoutes
              association={association}
              url={newUrl}
              index={newIndex}
            />
          );
        }}
      />
      <Route path={makePath('')}>
        <Redirect to={getGraphPermalink(association, group, index)} />
      </Route>
    </Switch>
  );
}
