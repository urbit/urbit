import React from 'react';
import { Route, Switch } from 'react-router-dom';

import LinksApp from '../apps/links/app';
import PublishApp from '../apps/publish/app';
import ChatApp from '../apps/chat/app';


export const TwoPaneApp = (props) => {
  return (
    <Switch>
      <Route
        path='/~chat'
        render={p => (
          <ChatApp
            location={p.location}
            match={p.match}
            history={p.history}
            {...props} />
        )}
      />
      <Route
        path='/~link'
        render={p => (
          <LinksApp
            location={p.location}
            match={p.match}
            history={p.history}
            {...props} />
        )}
      />
      <Route
        path='/~publish'
        render={p => (
          <PublishApp
            location={p.location}
            match={p.match}
            history={p.history}
            {...props} />
        )}
      />
    </Switch>
  );
}
