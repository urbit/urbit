import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import classnames from 'classnames';

import { Popout } from './components/lib/icons/popout';
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
    this.resetControllers();
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    document.title = 'OS1 - Dojo';

    const channel = new this.props.channel();
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
      <div
        className="bg-white bg-gray0-d"
        style={{ height: 'calc(100vh - 45px)' }}
      >
        <Route
          exact
          path="/~dojo/:popout?"
          render={(props) => {
            const popout = Boolean(props.match.params.popout);

            const popoutClasses = classnames({
              'mh4-m mh4-l mh4-xl': !popout,
              'mb4-m mb4-l mb4-xl': !popout,
              'ba-m ba-l ba-xl': !popout
            });

            return (
              <div className="w-100 h-100 flex-m flex-l flex-xl">
                <div
                  className="db dn-m dn-l dn-xl inter bg-white bg-gray0-d dt w-100"
                  style={{ height: 40 }}
                >
                </div>
                <div
                  className={
                    'pa3 bg-white bg-gray0-d black white-d mono w-100 f8 relative' +
                    ' h-100-m40-s b--gray2 br1 flex-auto flex flex-column ' +
                    popoutClasses
                  }
                  style={{
                    lineHeight: '1.4',
                    cursor: 'text'
                  }}
                >
                  <Popout popout={popout} />
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
    );
  }
}
