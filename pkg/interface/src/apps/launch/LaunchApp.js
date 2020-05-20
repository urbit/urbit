import React from 'react';

import LaunchApi from '../../api/launch';
import LaunchStore from '../../store/launch';
import LaunchSubscription from '../../subscription/launch';

import './css/custom.css';


export default class LaunchApp extends React.Component {
  constructor(props) {
    super(props);
    this.store = new LaunchStore();
    this.store.setStateHandler(this.setState.bind(this));

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
    new Image().src = '/~chat/img/Spinner.png';

    this.store.clear();
    const channel = new this.props.channel();
    this.api = new LaunchApi(this.props.ship, channel, this.store);

    this.subscription = new LaunchSubscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.resetControllers();
  }

  render() {
    return (
      <div></div>
    );
  }
}

