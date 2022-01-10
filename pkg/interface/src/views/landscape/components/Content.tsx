import { Box } from '@tlon/indigo-react';
import React, { useCallback, useEffect } from 'react';
import { Route, Switch, useHistory, useLocation } from 'react-router-dom';
import styled from 'styled-components';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import useMetadataState from '~/logic/state/metadata';
import LaunchApp from '~/views/apps/launch/App';
import Notifications from '~/views/apps/notifications/notifications';
import { PermalinkRoutes } from '~/views/apps/permalinks/app';
import Profile from '~/views/apps/profile/profile';
import Settings from '~/views/apps/settings/settings';
import ErrorComponent from '~/views/components/Error';
import { useShortcut } from '~/logic/state/settings';

import Landscape from '~/views/landscape/index';
import GraphApp from '../../apps/graph/App';
import { getNotificationRedirect } from '~/logic/lib/notificationRedirects';
import {JoinRoute} from './Join/Join';
import useInviteState from '~/logic/state/invite';

export const Container = styled(Box)`
   flex-grow: 1;
   overflow: hidden;
   width: 100%;
   height: calc(100% - 62px);
`;

export const Content = (props) => {
  const history = useHistory();
  const location = useLocation();
  const mdLoaded = useMetadataState(s => s.loaded);
  const inviteLoaded = useInviteState(s => s.loaded);

  useEffect(() => {
    const query = new URLSearchParams(location.search);
    if(!(mdLoaded && inviteLoaded)) {
      return;
    }
    if(query.has('grid-note')) {
      history.push(getNotificationRedirect(query.get('grid-note')!));
    } else if(query.has('grid-link')) {
      const link = decodeURIComponent(query.get('grid-link')!);
      history.push(`/perma${link}`);
    }
  }, [location.search, mdLoaded, inviteLoaded]);

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
      <JoinRoute />
      <Switch>
        <Route
          exact
          path="/"
        >
          <Landscape />
        </Route>
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
