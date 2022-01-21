/* eslint-disable max-lines-per-function */
import { Box, Icon, Row, Text, Button } from "@tlon/indigo-react";
import React, { ReactElement } from "react";
import { Helmet } from "react-helmet";
import { Route, useHistory } from "react-router-dom";
import styled from "styled-components";
import useHarkState from "~/logic/state/hark";
import useSettingsState, { selectCalmState } from "~/logic/state/settings";
import Groups from "./components/Groups";
import { NewGroup } from "~/views/landscape/components/NewGroup";
import ModalButton from "./components/ModalButton";
import Tiles from "./components/tiles";
import Tile from "./components/tiles/tile";
import "./css/custom.css";
import { createJoinParams, Join, JoinRoute } from "~/views/landscape/components/Join/Join";

export const ScrollbarLessBox = styled(Box)`
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
  const history = useHistory();

  return (
    <>
      <Helmet defer={false}>
        <title>
          {notificationsCount ? `(${String(notificationsCount)}) ` : ''}EScape
        </title>
      </Helmet>
      <Route path="/join/:ship/:name">
        <JoinRoute />
      </Route>
      <ScrollbarLessBox
        height="100%"
        overflowY="scroll"
        display="flex"
        flexDirection="column"
      >
        <Box
          mx={2}
          display="grid"
          gridTemplateColumns="repeat(auto-fill, minmax(128px, 1fr))"
          gridGap={3}
          p={2}
          pt={0}
        >
          {!hideUtilities && (
            <>
              <Tile
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
                onClick={() => history.push({ search: createJoinParams('groups') })}
              >
                <Row backgroundColor="white" gapX="2" p={2} height="100%" width="100%" alignItems="center">
                  <Icon icon="BootNode" />
                  <Text fontWeight="medium" whiteSpace="nowrap">Join Group</Text>
                </Row>
              </Button>
            </>
          )}
          {!hideGroups && <Groups />}
        </Box>
      </ScrollbarLessBox>
    </>
  );
};

export default LaunchApp;
