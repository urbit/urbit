import React from 'react';
import { useHistory } from 'react-router-dom';
import { Box, Text, Row, Button, Icon, H2, Col } from '@tlon/indigo-react';
import { NewGroup } from '~/views/landscape/components/NewGroup';
import ModalButton from '~/views/apps/launch/components/ModalButton';
import Tile from '~/views/apps/launch/components/tiles/tile';
import { ScrollbarLessBox } from '~/views/apps/launch/App';
import useHarkState from "~/logic/state/hark";
import { NewBox } from '~/views/apps/notifications/NewBox';
import { version } from '~/../package.json';

export function UqbarHome(props) {
  const history = useHistory();
  const { notificationsCount } = useHarkState();

  return (
    <ScrollbarLessBox
      height="100%"
      overflowY="scroll"
      display="flex"
      flexDirection="column"
      mt={3}
    >
      <H2 mb={3} ml={3}>EScape from Eternal September <Text fontSize="16px">(v{version})</Text></H2>
      {notificationsCount === 0 && <Text ml={3}>No notifications</Text>}
      <NewBox hideLabel />
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
        </Tile>
        <Tile>
          <Col display="flex" alignItems="center" justifyContent="center" height="100%">
            <ModalButton
              icon="CreateGroup"
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
                <Icon icon="Plus" />
                <Text fontWeight="medium" whiteSpace="nowrap">Join Group</Text>
              </Row>
            </Button>
          </Col>
        </Tile> */}
      </Box>
    </ScrollbarLessBox>
  );
}
