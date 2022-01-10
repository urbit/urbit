import React from 'react';
import { useHistory } from 'react-router-dom';
import { Box, Image, Text, Row, Button, Icon } from '@tlon/indigo-react';
import { NewGroup } from '~/views/landscape/components/NewGroup';
import ModalButton from '~/views/apps/launch/components/ModalButton';
import Tiles from '~/views/apps/launch/components/tiles';
import Tile from '~/views/apps/launch/components/tiles/tile';
import { ScrollbarLessBox } from '~/views/apps/launch/App';
import UqbarLogo from '~/assets/img/uqbar-logo.jpeg';

export function UqbarHome(props) {
  const history = useHistory();

  return (
    <ScrollbarLessBox
      height="100%"
      overflowY="scroll"
      display="flex"
      flexDirection="column"
      mt={3}
    >
      <Box
        mx={2}
        display="grid"
        gridTemplateColumns="repeat(auto-fill, minmax(128px, 1fr))"
        gridGap={3}
        p={2}
        pt={0}
      >
        {/* <Tile
          bg="white"
          color="scales.black20"
          to="/~landscape/home"
          p={0}
        >
          <Box
            p={2}
            height="100%"
            width="100%"
            bg="scales.black20"
            border={1}
            borderColor="lightGray"
          >
            <Row alignItems="center">
              <Icon color="black" icon="Home" />
              <Text ml={2} mt="1px" color="black">
                My Channels
              </Text>
            </Row>
          </Box>
        </Tile> */}
        <Tile p="0">
          <Image
            referrerPolicy="no-referrer"
            flexShrink={0}
            src={UqbarLogo}
            height="100%"
            width="100%"
            // objectFit="contain"
            onError={console.warn}
          />
        </Tile>
        <Tiles />
        <ModalButton
          icon="Plus"
          bg="white"
          color="black"
          text="New Group"
          style={{ gridColumnStart: 1 }}
        >
          <NewGroup />
        </ModalButton>
        <Button
          border={0}
          p={0}
          borderRadius={2}
          onClick={() => history.push({ search: '?join-kind=group' })}
        >
          <Row backgroundColor="white" gapX="2" p={2} height="100%" width="100%" alignItems="center">
            <Icon icon="BootNode" />
            <Text fontWeight="medium" whiteSpace="nowrap">Join Group</Text>
          </Row>
        </Button>
      </Box>
    </ScrollbarLessBox>
  );
}
