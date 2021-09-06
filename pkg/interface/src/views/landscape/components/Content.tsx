import { Box } from '@tlon/indigo-react';
import React, { useCallback, useEffect } from 'react';
import { Route, Switch, useHistory, useLocation } from 'react-router-dom';
import styled from 'styled-components';
import ob from 'urbit-ob';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import useMetadataState from '~/logic/state/metadata';
import LaunchApp from '~/views/apps/launch/App';
import Notifications from '~/views/apps/notifications/notifications';
import { PermalinkRoutes } from '~/views/apps/permalinks/app';
import Profile from '~/views/apps/profile/profile';
import Settings from '~/views/apps/settings/settings';
import TermApp from '~/views/apps/term/app';
import ErrorComponent from '~/views/components/Error';
import { useShortcut } from '~/logic/state/settings';

import Landscape from '~/views/landscape/index';
import GraphApp from '../../apps/graph/App';

export const Container = styled(Box)`
   flex-grow: 1;
   overflow: hidden;
   width: 100%;
   height: calc(100% - 62px);
`;

function getGroupResourceRedirect(key: string) {
  const association = useMetadataState.getState().associations.graph[`/ship/${key}`];
  const { metadata } = association;
  if(!association || !('graph' in metadata.config)) {
    return '';
  }
  return `/~landscape${association.group}/resource/${metadata.config.graph}${association.resource}`;
}

function getPostRedirect(key: string, segs: string[]) {
  const association = useMetadataState.getState().associations.graph[`/ship/${key}`];
  const { metadata } = association;
  if(!association || !('graph' in metadata.config)) {
    return '';
  }
  return `/~landscape${association.group}/feed/thread/${segs.slice(0, -1).join('/')}`;
}

function getChatRedirect(chat: string, segs: string[]) {
  const qs = segs.length > 0 ? `?msg=${segs[0]}` : '';
  return `${getGroupResourceRedirect(chat)}${qs}`;
}

function getPublishRedirect(graphKey: string, segs: string[]) {
  const base = getGroupResourceRedirect(graphKey);
  if(segs.length === 3) {
    return `${base}/note/${segs[0]}`;
  } else if (segs.length === 4) {
    return `${base}/note/${segs[0]}?selected=${segs[2]}`;
  }
  return base;
}

function getLinkRedirect(graphKey: string, segs: string[]) {
  const base = getGroupResourceRedirect(graphKey);
  if(segs.length === 1) {
    return `${base}/index/${segs[0]}`;
  } else if (segs.length === 3) {
    return `${base}/index/${segs[0]}?selected=${segs[1]}`;
  }
  return base;
}

function getNotificationRedirect(link: string) {
  const [,mark, ship, name, ...rest] = link.split('/');
  const graphKey = `${ship}/${name}`;
  switch(mark) {
    case 'graph-validator-dm':
      return `/~landscape/messages/dm/${ob.patp(rest[0])}`;
    case 'graph-validator-chat':
      return getChatRedirect(graphKey, rest);
    case 'graph-validator-publish':
      return getPublishRedirect(graphKey, rest);
    case 'graph-validator-link':
    return getLinkRedirect(graphKey, rest);
    case 'graph-validator-post':
      return getPostRedirect(graphKey, rest);
    default:
      return'';
  }
}

export const Content = (props) => {
  const history = useHistory();
  const location = useLocation();
  const associations = useMetadataState(s => s.associations.graph);

  useEffect(() => {
    const query = new URLSearchParams(location.search);
    if(Object.keys(associations).length > 0 && query.has('grid-note')) {
      history.push(getNotificationRedirect(query.get('grid-note')));
      console.log(query.get('grid-note'));
    }
  }, [location.search]);

  useShortcut('navForward', useCallback((e) => {
    e.preventDefault();
    e.stopImmediatePropagation();
    history.goForward();
  }, [history.goForward]));

  useShortcut('navBack', useCallback((e) => {
    e.preventDefault();
    e.stopImmediatePropagation();
    history.goBack();
  }, [history.goBack]));

  const [hasProtocol, setHasProtocol] = useLocalStorageState(
    'registeredProtocol', false
  );

  useEffect(() => {
    if(!hasProtocol && window?.navigator?.registerProtocolHandler) {
      try {
        window.navigator.registerProtocolHandler('web+urbitgraph', '/perma?ext=%s', 'Urbit Links');
        console.log('registered protocol');
        setHasProtocol(true);
      } catch (e) {
        console.log(e);
      }
    }
  }, [hasProtocol]);

  return (
    <Container>
      <Switch>
        <Route
          exact
          path='/'
          render={p => (
            <LaunchApp
              location={p.location}
              match={p.match}
              {...props}
            />
          )}
        />
        <Route
          path='/~term'
          render={p => (
            <TermApp
              location={p.location}
              match={p.match}
              {...props}
            />
          )}
        />
        <Route path='/~landscape'>
          <Landscape />
        </Route>
        <Route
          path="/~profile"
          render={ p => (
            <Profile
             {...props}
            />
          )}
        />
        <Route
          path="/~settings"
          render={ p => (
            <Settings {...props} />
          )}
        />
        <Route
          path="/~notifications"
          render={ p => (
            <Notifications {...props} />
          )}
        />
        <GraphApp path="/~graph" {...props} />
        <PermalinkRoutes {...props} />
        <Route
          render={p => (
            <ErrorComponent
              code={404}
              description="Not Found"
              {...p}
            />
          )}
        />
      </Switch>
    </Container>
  );
};
