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
        <div>
        <Route exact path="/~contacts"
          render={ (props) => {
            return (
              <Skeleton display="groups">
                <div className="h-100 w-100 overflow-x-hidden flex flex-column">
                </div>
              </Skeleton>
            );
          }} />
        </div>
      </BrowserRouter>
    )
  }
}

