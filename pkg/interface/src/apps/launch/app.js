import React from 'react';

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
      <div className="h-100 flex flex-column h-100">
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
      <div className="absolute mono bottom-0 left-0 f9 gray2 ml4 mb4 f8"> {props.baseHash} </div>
    </div>
    );
  }
}

