/* eslint-disable max-lines-per-function */
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { Helmet } from 'react-helmet';
import { Route } from 'react-router-dom';
import styled from 'styled-components';
import useHarkState from '~/logic/state/hark';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { JoinGroup } from '~/views/landscape/components/JoinGroup';
import { NewGroup } from '~/views/landscape/components/NewGroup';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import { Invite } from './components/Invite';
import './css/custom.css';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

interface LaunchAppProps {
  connection: string;
}

export const LaunchApp = (props: LaunchAppProps): ReactElement | null => {
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const calmState = useSettingsState(selectCalmState);
  const { hideUtilities, hideGroups } = calmState;

  return (
    <>
      <Helmet defer={false}>
        <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Groups</title>
      </Helmet>
      <Route path="/invites/:app/:uid">
        <Invite />
      </Route>
      <ScrollbarLessBox height='100%' overflowY='scroll' display="flex" flexDirection="column">
        <Box
          mx={2}
          display='grid'
          gridTemplateColumns='repeat(auto-fill, minmax(128px, 1fr))'
          gridGap={3}
          p={2}
          pt={0}
        >
        {!hideUtilities && <>
          <Tile
            bg="white"
            color="scales.black20"
            to="/~landscape/home"
            p={0}
          >
            <Box
              p={2}
              height='100%'
              width='100%'
              bg='scales.black20'
              border={1}
              borderColor="lightGray"
            >
              <Row alignItems='center'>
                <Icon
                  color="black"
                  icon="Home"
                />
                <Text ml={2} mt='1px' color="black">My Channels</Text>
              </Row>
            </Box>
          </Tile>
          <Tiles />
          <ModalButton
            icon="Plus"
            bg="washedGray"
            color="black"
            text="New Group"
            style={{ gridColumnStart: 1 }}
          >
            <NewGroup />
          </ModalButton>
          <ModalButton
            icon="BootNode"
            bg="washedGray"
            color="black"
            text="Join Group"
          >
            {dismiss => <JoinGroup dismiss={dismiss} />}
          </ModalButton>
          </>}
          {!hideGroups &&
            (<Groups />)
          }
        </Box>
      </ScrollbarLessBox>
    </>
  );
};

export default LaunchApp;
