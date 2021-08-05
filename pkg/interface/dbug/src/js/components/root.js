import React, { Component } from 'react';
import { BrowserRouter, Switch, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { MessageScreen } from '/components/message-screen';
import { Apps } from '/views/apps';
import { Spider } from '/views/spider';
import { Ames } from '/views/ames';
import { Behn } from '/views/behn';
import { Clay } from '/views/clay';
import { Eyre } from '/views/eyre';
import { makeRoutePath } from '../lib/util';

export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    // preload spinner asset
    //new Image().src = "/~debug/img/Spinner.png";
  }

  render() {
    const { state } = this;

    return (
      <BrowserRouter basename="/apps/debug"><Switch>
        <Route exact path="/"
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="">
                <MessageScreen text="select a component on the left" />
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('apps')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="apps">
                <Apps apps={state.apps} {...props}/>
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('spider')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="spider">
                <Spider threads={state.threads} {...props}/>
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('ames')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="ames">
                <Ames peers={state.peers} {...props}/>
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('behn')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="behn">
                <Behn timers={state.timers} {...props}/>
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('clay')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="clay">
                <Clay commits={state.commits} {...props}/>
              </Skeleton>
            );
          }}
        />

        <Route exact path={makeRoutePath('eyre')}
          render={(props) => {
            return (
              <Skeleton status={state.status} selected="eyre">
                <Eyre
                  bindings={state.bindings}
                  connections={state.connections}
                  authentication={state.authentication}
                  channels={state.channels}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />

      </Switch></BrowserRouter>
    )
  }
}
