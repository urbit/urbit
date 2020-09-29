import React from 'react';
import Helmet from 'react-helmet';
import { Link } from 'react-router-dom';

import { Box, Row, Icon, Text } from '@tlon/indigo-react';

import './css/custom.css';

import Tiles from './components/tiles';
import Welcome from './components/welcome';
import Groups from './components/Groups';

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
          <Welcome firstTime={props.launch.firstTime} api={props.api} />
          <Row flexWrap="wrap" mb={4} pitch={4}>
            <Box 
              border={1}
              borderRadius={1}
              borderColor="lightGray"
              m={2}
              bg="white"
              width="126px"
              height="126px"
            >
              <Link to='/~groups/home'>
                <Box p={2} bg="washedGreen" width="100%" height="100%">
                  <Row alignItems="center">
                    <Icon
                      stroke="green"
                      fill="rgba(0,0,0,0)"
                      icon="Circle" 
                    />
                    <Text ml="1" color="green">Home</Text>
                  </Row>
                </Box>
              </Link>
            </Box>
            <Tiles
              tiles={props.launch.tiles}
              tileOrdering={props.launch.tileOrdering}
              api={props.api}
              location={props.userLocation}
              weather={props.weather}
            />
          </Row>
          <Groups associations={props.associations} />
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

