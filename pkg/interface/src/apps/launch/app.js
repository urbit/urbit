import React from 'react';

import LaunchApi from '../../api/launch';
import LaunchStore from '../../store/launch';
import LaunchSubscription from '../../subscription/launch';

import './css/custom.css';

import Tiles from './components/tiles';
import Welcome from './components/welcome';


export default class LaunchApp extends React.Component {
  constructor(props) {
    super(props);
    this.store = new LaunchStore();
    this.state = this.store.state;
    this.resetControllers();
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    window.title = 'OS1 - Home';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.store.setStateHandler(this.setState.bind(this));
    const channel = new this.props.channel();
    this.api = new LaunchApi(this.props.ship, channel, this.store);

    this.subscription = new LaunchSubscription(this.store, this.api, channel);
    this.subscription.start();
    window.api = this.api;
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.store.setStateHandler(() => {});
    this.resetControllers();
  }

  render() {
    const { state } = this;

    return (
      <div style={{ height: 'calc(100vh - 45px)'}}
           className='h-100 w-100 bg-gray0-d'>
        <Welcome firstTime={state.launch.firstTime} api={this.api} />
        <Tiles
          tiles={state.launch.tiles}
          tileOrdering={state.launch.tileOrdering}
          api={this.api} />
      </div>
    );
  }
}

