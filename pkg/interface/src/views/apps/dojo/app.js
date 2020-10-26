import React, { Component } from 'react';
import { Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import { History } from './components/history';
import { Input } from './components/input';

import Api from './api';
import Store from './store';
import Subscription from './subscription';

import './css/custom.css';

export default class DojoApp extends Component {
  constructor(props) {
    super(props);
    this.store = new Store();
    this.store.setStateHandler(this.setState.bind(this));

    this.state = this.store.state;
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    this.resetControllers();
    const channel = new window.channel();
    this.api = new Api(this.props.ship, channel);
    this.store.api = this.api;

    this.subscription = new Subscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.resetControllers();
  }

  render() {
    return (
      <>
        <Helmet>
          <title>OS1 - Dojo</title>
        </Helmet>
        <div
          style={{ height: '100%' }}
        >
          <Route
            exact
            path="/~dojo/"
            render={(props) => {
              return (
                <div className="w-100 h-100 flex-m flex-l flex-xl">
                  <div
                    className="db dn-m dn-l dn-xl inter dt w-100"
                    style={{ height: 40 }}
                  >
                  </div>
                  <div
                    className={
                      'pa3 bg-white bg-gray0-d black white-d mono w-100 f8 relative' +
                      ' h-100-m40-s b--gray2 br1 flex-auto flex flex-column ' +
                      'mh4-m mh4-l mh4-xl mb4-m mb4-l mb4-xl ba-m ba-l ba-xl'
                    }
                    style={{
                      lineHeight: '1.4',
                      cursor: 'text'
                    }}
                  >
                    <History commandLog={this.state.txt} />
                    <Input
                      ship={this.props.ship}
                      cursor={this.state.cursor}
                      prompt={this.state.prompt}
                      input={this.state.input}
                      api={this.api}
                      store={this.store}
                    />
                  </div>
                </div>
              );
            }}
          />
        </div>
      </>
    );
  }
}
