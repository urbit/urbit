import React, { Component } from 'react';
import { BrowserRouter, Route, Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { subscription } from '/subscription';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { base64urlDecode } from '../lib/util';


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

    const groups = !!state.groups ? state.groups : {};

    return (
      <BrowserRouter>
        <Route exact path="/~eth-wallet"
          render={ (props) => {
            return (
              <Skeleton
                groups={groups}
                rightPanelHide={true}
                sidebarShown={true}
                >
                <div className="h-100 w-50 overflow-x-hidden flex flex-column bg-white bg-gray0-d dn db-ns">
                <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                      <p className="f8 pt3 gray2 w-50 h-100 dtc v-mid tc">
                        Transaction history.
                      </p>
                    </div>
                </div>
              </Skeleton>
            );
          }} />
      </BrowserRouter>
    )
  }
}