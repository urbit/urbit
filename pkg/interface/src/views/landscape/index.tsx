import { Box } from '@tlon/indigo-react';
import moment from 'moment';
import React from 'react';
import Helmet from 'react-helmet';
import { Route, Switch } from 'react-router-dom';
import useHarkState from '~/logic/state/hark';
import { Workspace } from '~/types/workspace';
import { Body } from '../components/Body';
import { GroupsPane } from './components/GroupsPane';
import { JoinGroup } from './components/JoinGroup';
import { NewGroup } from './components/NewGroup';
import './css/custom.css';
import _ from 'lodash';

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

const makeGroupWorkspace = _.memoize((group: string): Workspace => ({ type: 'group', group }));

const homeWorkspace: Workspace = { type: 'home' };
const messagesWorkspace: Workspace = { type: 'messages' };

export default function Landscape() {
  const notificationsCount = useHarkState(s => s.notificationsCount);

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
            const ws: Workspace = makeGroupWorkspace(groupPath);

            return (
              <GroupsPane workspace={ws} baseUrl={baseUrl} />
            );
          }}
        />
        <Route path="/~landscape/home">
          <GroupsPane workspace={homeWorkspace} baseUrl="/~landscape/home" />
        </Route>
        <Route path="/~landscape/messages">
          <GroupsPane workspace={messagesWorkspace} baseUrl="/~landscape/messages" />
        </Route>
        <Route path="/~landscape/new">
          <Body>
            <Box maxWidth="300px">
              <NewGroup />
            </Box>
          </Body>
        </Route>
        <Route path="/~landscape/join/:ship?/:name?"
          render={(routeProps) => {
            const { ship, name } = routeProps.match.params;
            const autojoin = ship && name ? `${ship}/${name}` : undefined;
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
