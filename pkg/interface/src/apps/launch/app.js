import React from 'react';

import LaunchApi from '../../api/launch';
import LaunchStore from '../../store/launch';
import LaunchSubscription from '../../subscription/launch';

import './css/custom.css';

import Tiles from './components/tiles';
import Welcome from './components/welcome';

export default class LaunchApp extends React.Component {

  componentDidMount() {
    document.title = 'OS1 - Home';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

  }

  componentWillUnmount() {}

  render() {
    const { props } = this;

    return (
      <div className='v-mid ph2 dtc-m dtc-l dtc-xl flex justify-between flex-wrap' style={{ maxWidth: '40rem' }}>
        <Welcome firstTime={props.launch.firstTime} api={props.api} />
        <Tiles
          tiles={props.launch.tiles}
          tileOrdering={props.launch.tileOrdering}
          api={props.api}
          location={props.userLocation}
          weather={props.weather}
        />
      </div>
    );
  }
}

