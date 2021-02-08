import React, { useState, useEffect, useCallback } from "react";
import { Box, Col, Row, Button, Text, Icon, Action } from "@tlon/indigo-react";
import { useHistory } from "react-router-dom";
import { TutorialProgress, tutorialProgress as progress } from "~/types";

import { Portal } from "~/views/components/Portal";
import useLocalState, { selectLocalState } from "~/logic/state/local";
import {
  progressDetails,
  MODAL_HEIGHT_PX,
  MODAL_WIDTH_PX,
  MODAL_WIDTH,
  MODAL_HEIGHT,
  TUTORIAL_HOST,
  TUTORIAL_GROUP,
} from "~/logic/lib/tutorialModal";
import { getRelativePosition } from "~/logic/lib/relativePosition";
import { StatelessAsyncButton } from "~/views/components/StatelessAsyncButton";
import GlobalApi from "~/logic/api/global";

const localSelector = selectLocalState([
  "tutorialProgress",
  "nextTutStep",
  "prevTutStep",
  "tutorialRef",
  "hideTutorial",
]);

export function TutorialModal(props: { api: GlobalApi }) {
  const {
    tutorialProgress,
    tutorialRef,
    nextTutStep,
    prevTutStep,
    hideTutorial,
  } = useLocalState(localSelector);
  const {
    title,
    description,
    alignX,
    alignY,
    offsetX,
    offsetY,
  } = progressDetails[tutorialProgress];

  const [coords, setCoords] = useState({});

  const history = useHistory();

  const next = useCallback(
    (e: React.MouseEvent<HTMLElement, MouseEvent>) => {
      e.stopPropagation();
      const idx = progress.findIndex((p) => p === tutorialProgress);
      const { url } = progressDetails[progress[idx + 1]];
      history.push(url);
      nextTutStep();
    },
    [nextTutStep, history, tutorialProgress, setCoords]
  );
  const prev = useCallback(() => {
    const idx = progress.findIndex((p) => p === tutorialProgress);
    history.push(progressDetails[progress[idx - 1]].url);
    prevTutStep();
  }, [prevTutStep, history, tutorialProgress]);

  const updatePos = useCallback(() => {
    const newCoords = getRelativePosition(
      tutorialRef,
      alignX,
      alignY,
      offsetX,
      offsetY
    );
    if (newCoords) {
      setCoords(newCoords);
    }
  }, [tutorialRef]);

  const dismiss = useCallback(() => {
    hideTutorial();
    props.api.settings.putEntry("tutorial", "seen", true);
  }, [hideTutorial, props.api]);

  const leaveGroup = useCallback(async () => {
    await props.api.groups.leaveGroup(TUTORIAL_HOST, TUTORIAL_GROUP);
  }, [props.api]);

  const progressIdx = progress.findIndex((p) => p === tutorialProgress);

  useEffect(() => {
    if (
      tutorialProgress !== "hidden" &&
      tutorialProgress !== "done" &&
      tutorialRef
    ) {
      const interval = setInterval(updatePos, 100);
      return () => {
        clearInterval(interval);
      };
    }
    return () => {};
  }, [tutorialRef, tutorialProgress, updatePos]);

  //  manually center final window
  useEffect(() => {
    if (tutorialProgress === "done") {
      const { innerWidth, innerHeight } = window;
      const left = `${(innerWidth - MODAL_WIDTH) / 2}px`;
      const top = `${(innerHeight - MODAL_HEIGHT) / 2}px`;
      console.log(`resetting ${top} ${left}`);
      setCoords({ top, left });
    }
  }, [tutorialProgress]);

  if (tutorialProgress === "hidden") {
    return null;
  }

  return (
    <Portal>
      <Box
        position="fixed"
        {...coords}
        bg="white"
        zIndex={50}
        height={MODAL_HEIGHT_PX}
        width={MODAL_WIDTH_PX}
        borderRadius="2"
      >
        <Col
          position="relative"
          justifyContent="space-between"
          height="100%"
          width="100%"
          borderRadius="2"
          p="2"
          bg="lightBlue"
        >
          <Box
            right="8px"
            top="8px"
            position="absolute"
            cursor="pointer"
            onClick={dismiss}
          >
            <Icon icon="X" />
          </Box>
          <Text lineHeight="tall" fontWeight="medium">
            {title}
          </Text>
          <Text lineHeight="tall">{description}</Text>
          {tutorialProgress !== "done" ? (
            <Row justifyContent="space-between">
              <Action bg="transparent" onClick={prev}>
                <Icon icon="ArrowWest" />
              </Action>
              <Text>
                {progressIdx}/{progress.length - 1}
              </Text>
              <Action bg="transparent" onClick={next}>
                <Icon icon="ArrowEast" />
              </Action>
            </Row>
          ) : (
            <Row justifyContent="space-between">
              <StatelessAsyncButton primary onClick={leaveGroup}>
                Leave Group
              </StatelessAsyncButton>
              <Button onClick={dismiss}>Later</Button>
            </Row>
          )}
        </Col>
      </Box>
    </Portal>
  );
}
