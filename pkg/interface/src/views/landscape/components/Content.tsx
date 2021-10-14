import { Box } from '@tlon/indigo-react';
import React, { Suspense, useCallback, useEffect } from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import styled from 'styled-components';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { PermalinkRoutes } from '~/views/apps/permalinks/app';
import { useShortcut } from '~/logic/state/settings';
import { Loading } from '~/views/components/Loading';
import LaunchApp from '~/views/apps/launch/App';

export const Container = styled(Box)`
   flex-grow: 1;
   overflow: hidden;
   width: 100%;
   height: calc(100% - 62px);
`;

const Landscape = React.lazy(() => import('~/views/landscape/index'));
const Settings = React.lazy(() => import('~/views/apps/settings/settings'));
const Profile = React.lazy(() => import('~/views/apps/profile/profile'));
const Notifications = React.lazy(() => import('~/views/apps/notifications/notifications'));
const GraphApp = React.lazy(() => import('../../apps/graph/App'));
const ErrorComponent = React.lazy(() => import('~/views/components/Error'));

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
      <Suspense fallback={Loading}>
        <Switch>
          <Route
            exact
            path={['/', '/invites/:app/:uid']}
            render={p => (
              <LaunchApp
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
      </Suspense>
    </Container>
  );
};
