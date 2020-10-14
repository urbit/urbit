import React from 'react';
import Helmet from 'react-helmet';

import { Box } from '@tlon/indigo-react';

import './css/custom.css';

import Tiles from './components/tiles';
import Welcome from './components/welcome';

export default class LaunchApp extends React.Component {

  componentDidMount() {
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

  }

  render() {
    const { props } = this;

    return (
      <>
        <Helmet>
          <title>OS1 - Home</title>
        </Helmet>
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
          <Box 
            position="absolute"
            fontFamily="mono"
            left="0"
            bottom="0"
            color="gray"
            bg="white"
            ml={3}
            mb={3}
            borderRadius={2}
            fontSize={0}
            p={2}>
            {props.baseHash}
          </Box>
        </div>
      </>
    );
  }
}

