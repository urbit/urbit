import { Box } from '@tlon/indigo-react';
import { PatpNoSig } from '@urbit/api';
import moment from 'moment';
import React, { ReactElement, useCallback, useEffect } from 'react';
import Helmet from 'react-helmet';
import { Route, RouteComponentProps, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { cite } from '~/logic/lib/util';
import useGraphState from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import { StoreState } from '~/logic/store/type';
import GlobalSubscription from '~/logic/subscription/global';
import { Workspace } from '~/types/workspace';
import { Body } from '../components/Body';
import { Loading } from '../components/Loading';
import { GroupsPane } from './components/GroupsPane';
import { JoinGroup } from './components/JoinGroup';
import { NewGroup } from './components/NewGroup';
import './css/custom.css';

moment.updateLocale('en', {
  relativeTime : {
    future: '%s',
    past:   '%s',
    s  : '1s',
    ss : '%ds',
    m:  '1m',
    mm: '%dm',
    h:  '1h',
    hh: '%dh',
    d:  '1d',
    dd: '%dd',
    w:  '1w',
    ww: '%dw',
    M:  '1mo',
    MM: '%dmo',
    y:  '1y',
    yy: '%dy'
  }
});

type LandscapeProps = StoreState & {
  ship: PatpNoSig;
  api: GlobalApi;
  subscription: GlobalSubscription;
}

export default function Landscape(props) {
  const notificationsCount = useHarkState(s => s.notificationsCount);

  return (
    <>
      <Helmet defer={false}>
        <title>{ props.notificationsCount ? `(${String(props.notificationsCount) }) `: '' }Landscape</title>
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
                    api={props.api}
                    {...routeProps}
                  />
                </Box>
              </Body>
            );
          }}
        />
        <Route path="/~landscape/join/:ship?/:name?"
          render={(routeProps) => {
            const { ship, name } = routeProps.match.params;
            const autojoin = ship && name ? `${ship}/${name}` : undefined;
            return (
              <Body>
                <Box maxWidth="300px">
                  <JoinGroup
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
    </>
  );
}

