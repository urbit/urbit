import React from 'react';
import Helmet from 'react-helmet';
import styled from 'styled-components';

import { Box, Row, Icon, Text, Center } from '@tlon/indigo-react';
import { uxToHex, adjustHex } from '~/logic/lib/util';

import './css/custom.css';

import { Sigil } from '~/logic/lib/sigil';
import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import Welcome from './components/welcome';
import Groups from './components/Groups';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

export default class LaunchApp extends React.Component {
  render() {
    const { props } = this;

    return (
      <>
        <Helmet>
          <title>OS1 - Home</title>
        </Helmet>
        <ScrollbarLessBox height='100%' overflowY='scroll'>
          <Welcome firstTime={props.launch.firstTime} api={props.api} />
          <Box
            mx='2'
            display='grid'
            gridTemplateColumns='repeat(auto-fill, minmax(128px, 1fr))'
            gridGap={3}
            p={2}
            pt={0}
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
                    icon="Boot"
                  />
                  <Text ml="1" mt='1px' color="white">DMs + Drafts</Text>
                </Row>
              </Box>
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
        </ScrollbarLessBox>
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

