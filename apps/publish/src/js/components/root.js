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

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.collections;

    console.log("root.state", this.state);

    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <BrowserRouter>
        <div>
        <Route exact path="/~publish"
          render={ (props) => {
            return (
              <div className="cf h-100 w-100 absolute">
                <div className="fl w-100 h3">
                  <h1>Publish</h1>
                </div>
                <div className="fl flex w-100 h-100">
                  <div className="fl h-100 overflow-x-hidden" style={{ flexBasis: 400 }}>
                    <p className="fl w-100 h2 bb">
                      Latest
                    </p>
                  </div>
                  <div className="fl h-100 overflow-x-hidden" style={{ flexBasis: 400 }}>
                    <p className="fl w-100 h2 bb">
                      Subs
                    </p>
                    <CollectionList
                      list={this.state.subs}
                    />
                  </div>
                  <div className="fl h-100 overflow-x-hidden" style={{ flexBasis: 400 }}>
                    <p className="fl w-100 h2 bb">
                      Pubs
                    </p>
                      <CollectionList
                        list={this.state.pubs}
                      />
                  </div>
                  <div className="fl h-100 overflow-x-hidden" style={{ flexBasis: 400 }}>
                    <p className="fl w-100 h2 bb">
                      Create Button? idk
                    </p>
                  </div>
                </div>
              </div>
            );
         }} />
        </div>
      </BrowserRouter>
    )
  }
}

