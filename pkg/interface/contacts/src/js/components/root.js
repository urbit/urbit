import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';


export class Root extends Component {
  constructor(props) {
    super(props);

    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
    this.setSpinner = this.setSpinner.bind(this);
  }

  setSpinner(spinner) {
    this.setState({
      spinner
    });
  }

  render() {
    const { props, state } = this;

    return (
      <BrowserRouter>
        <div className="h-100 w-100">
        <Route exact path="/~contacts"
          render={ (props) => {
            return (
              <Skeleton activeDrawer="groups">
                <div className="h-100 w-100 overflow-x-hidden bg-gray0 dn db-ns"></div>
              </Skeleton>
            );
          }} />
        </div>
      </BrowserRouter>
    )
  }
}

