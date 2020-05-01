import React, { Component } from 'react';
import { Route } from 'react-router-dom';
import classnames from 'classnames';

import { Popout } from './components/lib/icons/popout';
import { History } from './components/history';
import { Input } from './components/input';

import store from './store';
import api from './api';
import subscription from './subscription';

import './css/custom.css';

export default class DojoApp extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    window.title = 'OS1 - Dojo';
    api.setAuthTokens(
      {
        ship: this.props.ship,
        dojoId: 'soto-' + Math.random().toString(36).substring(2)
      },
      this.props.channel
    );

    subscription.start(this.props.channel);
  }

  render() {
    return (
      <div
        className="bg-white bg-gray1-d"
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
                  <a className="f8 pl3 black white-d dtc h-100 v-mid" href="/">
                    ⟵ Landscape
                  </a>
                </div>
                <div
                  className={
                    'pa3 bg-white bg-gray0-d black white-d mono w-100 f8 relative' +
                    ' h-100-m40-s b--gray2 br1 flex-auto ' +
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
