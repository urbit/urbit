import React, { useState, useEffect, useCallback } from 'react';
import _ from 'lodash';
import { Box, Col, Row, Button, Text, Icon } from '@tlon/indigo-react';
import { useHistory } from 'react-router-dom';
import { TutorialProgress, tutorialProgress as progress } from '~/types';

import { Portal } from '~/views/components/Portal';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import {
  progressDetails,
  MODAL_HEIGHT_PX,
  MODAL_WIDTH_PX,
  MODAL_WIDTH,
  MODAL_HEIGHT,
  TUTORIAL_HOST,
  TUTORIAL_GROUP,
  getTrianglePosition
} from '~/logic/lib/tutorialModal';
import { getRelativePosition } from '~/logic/lib/relativePosition';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import GlobalApi from '~/logic/api/global';
import { Triangle } from '~/views/components/Triangle';
import { ModalOverlay } from '~/views/components/ModalOverlay';

const localSelector = selectLocalState([
  'tutorialProgress',
  'nextTutStep',
  'prevTutStep',
  'tutorialRef',
  'hideTutorial',
  'set'
]);

export function TutorialModal(props: { api: GlobalApi }) {
  const {
    tutorialProgress,
    tutorialRef,
    nextTutStep,
    prevTutStep,
    hideTutorial,
    set: setLocalState
  } = useLocalState(localSelector);
  const {
    title,
    description,
    arrow,
    alignX,
    alignY,
    offsetX,
    offsetY
  } = progressDetails[tutorialProgress];

  const [coords, setCoords] = useState({});
  const [paused, setPaused] = useState(false);

  const history = useHistory();

  const next = useCallback( () => {
      const idx = progress.findIndex(p => p === tutorialProgress);
      const { url } = progressDetails[progress[idx + 1]];
      nextTutStep();
      history.push(url);
    },
    [nextTutStep, history, tutorialProgress, setCoords]
  );
  const prev = useCallback(() => {
    const idx = progress.findIndex(p => p === tutorialProgress);
    prevTutStep();
    history.push(progressDetails[progress[idx - 1]].url);
  }, [prevTutStep, history, tutorialProgress]);

  const updatePos = useCallback(() => {
    const newCoords = getRelativePosition(
      tutorialRef,
      alignX,
      alignY,
      offsetX,
      offsetY
    );
    const withMobile: any = _.mapValues(newCoords, (value: string[], key: string) => {
      if(key === 'bottom' || key === 'left') {
        return ['0px', ...value];
      }
      return ['unset', ...value];
    });
    if(!('bottom' in withMobile)) {
      withMobile.bottom = ['0px', 'unset'];
    }
    if(!('left' in withMobile)) {
      withMobile.left = ['0px', 'unset'];
    }

    if (newCoords) {
      setCoords(withMobile);
    } else {
      setCoords({});
    }
  }, [tutorialRef]);

  const dismiss = useCallback(async () => {
    setPaused(false);
    hideTutorial();
    await props.api.settings.putEntry('tutorial', 'seen', true);
  }, [hideTutorial, props.api]);

  const bailExit = useCallback(() => {
    setPaused(false);
  }, []);

  const tryExit = useCallback(() => {
    setPaused(true);
  }, []);

  const leaveGroup = useCallback(async () => {
    await props.api.groups.leaveGroup(TUTORIAL_HOST, TUTORIAL_GROUP);
    await dismiss();
  }, [props.api, dismiss]);

  const progressIdx = progress.findIndex(p => p === tutorialProgress);

  useEffect(() => {
    if (
      tutorialProgress !== 'hidden' &&
      tutorialProgress !== 'done' &&
      tutorialRef
    ) {
      const interval = setInterval(updatePos, 100);
      return () => {
        setCoords({});
        clearInterval(interval);
      };
    }
    return () => {};
  }, [tutorialRef, tutorialProgress, updatePos]);

  const triPos = getTrianglePosition(arrow);

  if (tutorialProgress === 'done') {
    return (
      <Portal>
        <ModalOverlay dismiss={dismiss} borderRadius="2" maxWidth="270px" backgroundColor="white">
          <Col p="3" bg="lightBlue">
            <Col mb="3">
              <Text lineHeight="tall" fontWeight="bold">
                Tutorial Finished
              </Text>
              <Text fontSize="0" gray>
                {progressIdx} of {progress.length - 2}
              </Text>
            </Col>
            <Text lineHeight="tall">
              This tutorial is finished. Would you like to leave Beginner Island?
            </Text>
            <Row mt="3" gapX="2" justifyContent="flex-end">
              <Button backgroundColor="washedGray" onClick={dismiss}>
                Later
              </Button>
              <StatelessAsyncButton primary destructive onClick={leaveGroup}>
                Leave Group
              </StatelessAsyncButton>
            </Row>
          </Col>
        </ModalOverlay>
      </Portal>
    );
  }

  if (tutorialProgress === 'hidden') {
    return null;
  }

  if(paused) {
    return (
      <ModalOverlay dismiss={bailExit} borderRadius="2" maxWidth="270px" backgroundColor="white">
        <Col p="3">
          <Col mb="3">
            <Text lineHeight="tall" fontWeight="bold">
              End Tutorial Now?
            </Text>
          </Col>
          <Text lineHeight="tall">
            You can always restart the tutorial by typing "tutorial" in Leap.
          </Text>
          <Row mt="3" gapX="2" justifyContent="flex-end">
            <Button backgroundColor="washedGray" onClick={bailExit}>
              Cancel
            </Button>
            <StatelessAsyncButton primary destructive onClick={dismiss}>
              End Tutorial
            </StatelessAsyncButton>
          </Row>
        </Col>
      </ModalOverlay>

    );
  }

  if(Object.keys(coords).length === 0) {
    return null;
  }

  return (
    <Portal>
      <Box
        position="fixed"
        {...coords}
        bg="white"
        zIndex={50}
        display="flex"
        flexDirection="column"
        width={["100%", MODAL_WIDTH_PX]}
        borderRadius="2"
      >
        <Col
          position="relative"
          justifyContent="space-between"
          height="100%"
          width="100%"
          borderRadius="2"
          p="3"
          bg="lightBlue"

        >
          <Triangle
            {...triPos}
            position="absolute"
            size={16}
            color="lightBlue"
            direction={arrow}
            height="0px"
            width="0px"
            display={["none", "block"]}
          />

          <Box
            right="16px"
            top="16px"
            position="absolute"
            cursor="pointer"
            onClick={tryExit}
          >
            <Icon icon="X" />
          </Box>
          <Col mb="3">
            <Text lineHeight="tall" fontWeight="bold">
              {title}
            </Text>
            <Text fontSize="0" gray>
              {progressIdx} of {progress.length - 2}
            </Text>
          </Col>

          <Text lineHeight="tall">{description}</Text>
          <Row gapX="2" mt="3" justifyContent="flex-end">
            { progressIdx > 1 && (
              <Button bg="washedGray" onClick={prev}>
                Back
              </Button>
            )}
            <Button primary onClick={next}>
              Next
            </Button>
          </Row>
        </Col>
      </Box>
    </Portal>
  );
}
