import { Box } from '@tlon/indigo-react';
import React, { useCallback, useEffect } from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import styled from 'styled-components';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
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

export const Content = (props) => {
  const history = useHistory();

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

  useShortcut('leap', useCallback((e) => {
    e.preventDefault();
    e.stopImmediatePropagation();
    props.toggleOmnibox();
  }, [props.toggleOmnibox]));

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
