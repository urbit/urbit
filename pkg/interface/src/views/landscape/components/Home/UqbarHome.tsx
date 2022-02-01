import React from 'react';
import { useHistory } from 'react-router-dom';
import { Box, Image, Text, Row, Button, Icon, H2, Col, H3 } from '@tlon/indigo-react';
import { NewGroup } from '~/views/landscape/components/NewGroup';
import ModalButton from '~/views/apps/launch/components/ModalButton';
import Tile from '~/views/apps/launch/components/tiles/tile';
import { ScrollbarLessBox } from '~/views/apps/launch/App';
import { Sigil } from '~/logic/lib/sigil';
import { useDark } from '~/logic/state/join';
import { version } from '~/../package.json';

export function UqbarHome(props) {
  const history = useHistory();
  const dark = useDark();

  const sigilClass = dark ? 'mix-blend-diff' : 'mix-blend-darken';

  return (
    <ScrollbarLessBox
      height="100%"
      overflowY="scroll"
      display="flex"
      flexDirection="column"
      mt={3}
    >
      <H2 mb={3} ml={3}>EScape from Eternal September <Text fontSize="16px">(v{version})</Text></H2>
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
        <Tile>
          <Col display="flex" alignItems="center" justifyContent="center" height="100%">
            <Sigil
              ship={window.ship}
              size={60}
              display='block'
              color={'black'}
              classes={sigilClass}
              icon
              padding={12}
            />
            <Text mt={3} textAlign="center" fontWeight="bold">~{window.ship}</Text>
          </Col>
        </Tile>
        <Tile>
          <Col display="flex" alignItems="center" justifyContent="center" height="100%">
            <ModalButton
              icon="Plus"
              bg="white"
              color="black"
              text="New Group"
              style={{ gridColumnStart: 1 }}
            >
              <NewGroup />
            </ModalButton>
          </Col>
        </Tile>
        <Tile>
          <Col display="flex" alignItems="center" justifyContent="center" height="100%">
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
          </Col>
        </Tile>
      </Box>
    </ScrollbarLessBox>
  );
}
