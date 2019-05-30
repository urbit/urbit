import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import Mousetrap from 'mousetrap';
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { store } from '/store';
import { Skeleton } from '/components/skeleton';
import { Sidebar } from '/components/sidebar';
import { CollectionList } from '/components/collection-list';
import { Recent } from '/components/recent';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;

    console.log("root.state", this.state);

    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <BrowserRouter>
        <div>

        <Route exact path="/~publish/recent"
          render={ (props) => {
            return (
              <div className="cf w-100 absolute">
                <div className="fl w-100 h3 pl4 mt3">
                  <p className="body-large b gray-50">Publish</p>
                </div>
                <div className="fl flex w-100 bb pl4">
                  <div className="fl bb" style={{ flexBasis: 148 }}>
                    <p className="fl w-100 h2 label-regular">
                      Recent
                    </p>
                  </div>
                  <div className="fl" style={{ flexBasis: 148 }}>
                    <p className="fl w-100 h2 label-regular gray-30">
                      Subscriptions
                    </p>
                  </div>
                  <div className="fl" style={{ flexBasis: 148 }}>
                    <p className="fl w-100 h2 label-regular gray-30">
                      My Blogs
                    </p>
                  </div>
                </div>
                <div className="fl w-100">
                  <Recent
                    {...this.state}
                  />
                </div>
              </div>
            );
         }} />
        </div>
      </BrowserRouter>
    )
  }
}

