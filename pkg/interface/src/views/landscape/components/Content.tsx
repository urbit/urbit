import { Box } from '@tlon/indigo-react';
import React, { Suspense, useCallback, useEffect } from 'react';
import { Route, Switch, useHistory, useLocation } from 'react-router-dom';
import styled from 'styled-components';
import { PermalinkRoutes } from '~/views/apps/permalinks/app';
import { useShortcut } from '~/logic/state/settings';
import { Loading } from '~/views/components/Loading';
import LaunchApp from '~/views/apps/launch/App';
import Landscape from '~/views/landscape';
import Settings from '~/views/apps/settings/settings';
import Profile from '~/views/apps/profile/profile';
import Notifications from '~/views/apps/notifications/notifications';
import ErrorComponent from '~/views/components/Error';

import { getNotificationRedirectFromLink } from '~/logic/lib/notificationRedirects';
import { JoinRoute } from './Join/Join';
import useInviteState from '~/logic/state/invite';
import useMetadataState from '~/logic/state/metadata';

export const Container = styled(Box)`
   flex-grow: 1;
   overflow: hidden;
   width: 100%;
   height: calc(100% - 62px);
`;

export const Content = () => {
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
      history.push(getNotificationRedirectFromLink(query.get('grid-note')!));
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

  return (
    <Container>
      <Suspense fallback={Loading}>
        <JoinRoute />
        <Switch>
          <Route
            exact
            path="/"
            component={LaunchApp}
          />
          <Route path='/~landscape'>
            <Landscape />
          </Route>
          <Route
            path="/~profile"
            component={Profile}
          />
          <Route
            path="/~settings"
            component={Settings}
          />
          <Route
            path="/~notifications"
            component={Notifications}
          />
          <PermalinkRoutes />

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
