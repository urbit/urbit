import React, { useState } from 'react';
import Helmet from 'react-helmet';
import styled from 'styled-components';

import { Box, Row, Icon, Text } from '@tlon/indigo-react';

import './css/custom.css';

import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import Welcome from './components/welcome';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import { writeText } from '~/logic/lib/util';
import { NewGroup } from "~/views/landscape/components/NewGroup";
import { JoinGroup } from "~/views/landscape/components/JoinGroup";

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

export default function LaunchApp(props) {
  const [hashText, setHashText] = useState(props.baseHash);
  console.log('it works')
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
            bg="white"
            color="scales.black20"
            to="/~landscape/home"
            p={0}
          >
            <Box p={2} height='100%' width='100%' bg='scales.black20'>
              <Row alignItems='center'>
                <Icon
                  color="black"
                  icon="Mail"
                />
                <Text ml="1" mt='1px' color="black">DMs + Drafts</Text>
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
          <ModalButton
            icon="Plus"
            bg="blue"
            color="#fff"
            text="Join a Group"
            style={{ gridColumnStart: 1 }}
          >
            <JoinGroup {...props} />
          </ModalButton>
          <ModalButton
            icon="CreateGroup"
            bg="green"
            color="#fff"
            text="Create a Group"
          >
            <NewGroup {...props} />
          </ModalButton>
          <Groups unreads={props.unreads} groups={props.groups} associations={props.associations} />
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
        cursor="pointer"
        onClick={() => {
          writeText(props.baseHash);
          setHashText('copied');
          setTimeout(() => {
            setHashText(props.baseHash);
          }, 2000);
        }}
      >
        {hashText || props.baseHash}
      </Box>
    </>
  );
}
