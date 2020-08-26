import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom';
import styled from 'styled-components';

import { TwoPaneApp } from './TwoPaneApp';
import LaunchApp from '../apps/launch/app';
import DojoApp from '../apps/dojo/app';
import GroupsApp from '../apps/groups/app';
import Profile from '../apps/profile/profile';
import ErrorComponent from './Error';


export const Container = styled.div`
   height: calc(100% - 45px);
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
          path='/~groups'
          render={p => (
            <GroupsApp
              history={p.history}
              location={p.location}
              match={p.match}
              {...props} />
          )}
        />
        <Route
          path={[
            '/~chat',
            '/~publish',
            '/~link'
          ]}
          render={ p => (
            <TwoPaneApp
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
