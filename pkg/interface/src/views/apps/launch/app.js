import React from 'react';
import Helmet from 'react-helmet';
import { Link } from 'react-router-dom';

import { Box, Row, Icon, Text, Center } from '@tlon/indigo-react';
import { uxToHex } from "~/logic/lib/util";

import './css/custom.css';

import { Sigil } from "~/logic/lib/sigil";
import Tiles from './components/tiles';
import Welcome from './components/welcome';
import Groups from './components/Groups';

const Tile = ({ children, bg, to, ...rest }) => (
  <Box 
    m={2}
    bg="white"
    width="126px"
    height="126px"
    borderRadius={2}
    overflow="hidden"
    {...rest}>
    <Link to={to}>
      <Box p={2} bg={bg} width="100%" height="100%">
        {children}
      </Box>
    </Link>
  </Box>
);

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
      ? "#FFFFFF"
      : "#000000";


    return (
      <>
        <Helmet>
          <title>OS1 - Home</title>
        </Helmet>
        <div className="h-100 flex flex-column h-100">
          <Welcome firstTime={props.launch.firstTime} api={props.api} />
          <Row flexWrap="wrap" mb={4} pitch={4}>
            <Tile
              border={1}
              bg="washedGreen"
              borderColor="green"
              to="/~groups/home"
            >
              <Row alignItems="center">
                <Icon
                  stroke="green"
                  fill="rgba(0,0,0,0)"
                  icon="Circle" 
                />
                <Text ml="1" color="green">Home</Text>
              </Row>
            </Tile>
            <Tile
              bg={sigilColor}
              to="/~profile"
              borderRadius="3"
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

