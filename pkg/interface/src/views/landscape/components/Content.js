import React, { Component } from 'react';
import { Box } from '@tlon/indigo-react';
import { Route, Switch } from 'react-router-dom';
import styled from 'styled-components';

import LaunchApp from '~/views/apps/launch/app';
import DojoApp from '~/views/apps/dojo/app';
import Landscape from '~/views/landscape/index';
import Profile from '~/views/apps/profile/profile';
import ErrorComponent from '~/views/components/Error';


export const Container = styled(Box)`
   flex-grow: 1;
   overflow: hidden;
   width: 100%;
   height: calc(100% - 62px);
`;


export const Content = (props) => {
  return (
    <Container>
      <Switch>
        <Route
          exact
          path='/'
          render={p => (
            <LaunchApp
              history={p.history}
              location={p.location}
              match={p.match}
              {...props} />
          )}
        />
        <Route
          path='/~dojo'
          render={p => (
            <DojoApp
              history={p.history}
              location={p.location}
              match={p.match}
              {...props} />
          )}
        />
        <Route
          path='/~landscape'
          render={p => (
            <Landscape
              history={p.history}
              location={p.location}
              match={p.match}
              {...props} />
          )}
        />
        <Route
          path="/~profile"
          render={ p => (
            <Profile
             {...props}
            />
          )}
        />
        <Route
          render={p => (
            <ErrorComponent
              code={404}
              description="Not Found"
              {...p} />
          )}
        />
      </Switch>
    </Container>
  );
}
