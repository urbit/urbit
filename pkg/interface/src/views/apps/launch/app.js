import React from 'react';
import Helmet from 'react-helmet';

import { Box, Row, Icon, Text, Center } from '@tlon/indigo-react';
import { uxToHex, adjustHex } from '~/logic/lib/util';

import './css/custom.css';

import { Sigil } from '~/logic/lib/sigil';
import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import Welcome from './components/welcome';
import Groups from './components/Groups';

export default class LaunchApp extends React.Component {
  componentDidMount() {
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';
  }

  render() {
    const { props } = this;
    const contact = props.contacts?.['/~/default']?.[window.ship];
    const sigilColor = contact?.color
      ? `#${uxToHex(contact.color)}`
      : props.dark
      ? '#FFFFFF'
      : '#000000';

    return (
      <>
        <Helmet>
          <title>OS1 - Home</title>
        </Helmet>
        <Box height='100%' overflowY='scroll'>
          <Welcome firstTime={props.launch.firstTime} api={props.api} />
          <Box
            mx='2'
            display='grid'
            gridTemplateColumns='repeat(auto-fill, minmax(128px, 1fr))'
            gridGap={3}
            p={2}
          >
            <Tile
              bg="transparent"
              color="green"
              to="/~landscape/home"
              p={0}
            >
              <Box p={2} height='100%' width='100%' bg='green'>
                <Row alignItems='center'>
                  <Icon
                    color="white"
                    // fill="rgba(0,0,0,0)"
                    icon="Home"
                  />
                  <Text ml="1" mt='1px' color="white">Home</Text>
                </Row>
              </Box>
            </Tile>
            <Tile
              borderColor={adjustHex(sigilColor, -40)}
              bg={sigilColor}
              to="/~profile"
            >
              <Center height="100%">
                <Sigil ship={`~${window.ship}`} size={80} color={sigilColor} />
              </Center>
            </Tile>
            <Tiles
              tiles={props.launch.tiles}
              tileOrdering={props.launch.tileOrdering}
              api={props.api}
              location={props.userLocation}
              weather={props.weather}
            />
            <Box display={["none", "block"]} width="100%" gridColumn="1 / -1"></Box>
            <Groups groups={props.groups} associations={props.associations} invites={props.invites} api={props.api}/>
          </Box>
        </Box>
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
          p={2}
        >
          {props.baseHash}
        </Box>
      </>
    );
  }
}

