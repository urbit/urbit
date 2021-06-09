/* eslint-disable max-lines-per-function */
import { Box, Button, Col, Icon, Row, Text } from '@tlon/indigo-react';
import f from 'lodash/fp';
import React, { ReactElement, useEffect, useMemo, useState } from 'react';
import { Helmet } from 'react-helmet';
import styled from 'styled-components';
import GlobalApi from '~/logic/api/global';
import {
    hasTutorialGroup,

    TUTORIAL_BOOK,
    TUTORIAL_CHAT, TUTORIAL_GROUP,
    TUTORIAL_HOST,

    TUTORIAL_LINKS
} from '~/logic/lib/tutorialModal';
import { useModal } from '~/logic/lib/useModal';
import { useQuery } from '~/logic/lib/useQuery';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { writeText } from '~/logic/lib/util';
import useHarkState from '~/logic/state/hark';
import useLaunchState from '~/logic/state/launch';
import useLocalState from '~/logic/state/local';
import useMetadataState from '~/logic/state/metadata';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { StarIcon } from '~/views/components/StarIcon';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { JoinGroup } from '~/views/landscape/components/JoinGroup';
import { NewGroup } from '~/views/landscape/components/NewGroup';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import './css/custom.css';
import airlock from '~/logic/api';
import { join } from '@urbit/api/groups';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

const tutSelector = f.pick(['tutorialProgress', 'nextTutStep', 'hideGroups']);

interface LaunchAppProps {
  connection: string;
  api: GlobalApi;
}

export const LaunchApp = (props: LaunchAppProps): ReactElement | null => {
  const { connection } = props;
  const { baseHash, runtimeLag } = useLaunchState(state => state);
  const [hashText, setHashText] = useState(baseHash);
  const [exitingTut, setExitingTut] = useState(false);
  const seen = useSettingsState(s => s?.tutorial?.seen) ?? true;
  const associations = useMetadataState(s => s.associations);
  const hasLoaded = useMemo(() => Boolean(connection === 'connected'), [connection]);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const calmState = useSettingsState(selectCalmState);
  const { hideUtilities } = calmState;
  const { tutorialProgress, nextTutStep } = useLocalState(tutSelector);
  let { hideGroups } = useLocalState(tutSelector);
  !hideGroups ? { hideGroups } = calmState : null;

  const waiter = useWaitForProps({ ...props, associations });
  const hashBox = (
    <Box
      position={['relative', 'absolute']}
      left={0}
      bottom={0}
      backgroundColor="white"
      ml={3}
      mb={3}
      borderRadius={2}
      overflow='hidden'
      fontSize={0}
      cursor="pointer"
      onClick={() => {
        writeText(baseHash);
        setHashText('copied');
        setTimeout(() => {
          setHashText(baseHash);
        }, 2000);
      }}
    >
      <Box
        backgroundColor={runtimeLag ? 'yellow' : 'washedGray'}
        p={2}
      >
        <Text mono bold>{hashText || baseHash}</Text>
      </Box>
    </Box>
  );

  const { query } = useQuery();

  const { modal, showModal } = useModal({
    position: 'relative',
    maxWidth: '350px',
    modal: function modal(dismiss) {
      const onDismiss = (e) => {
        e.stopPropagation();
        props.api.settings.putEntry('tutorial', 'seen', true);
        dismiss();
      };
      const onContinue = async (e) => {
        e.stopPropagation();
        if (!hasTutorialGroup({ associations })) {
          await airlock.poke(join(TUTORIAL_HOST, TUTORIAL_GROUP));
          await props.api.settings.putEntry('tutorial', 'joined', Date.now());
          await waiter(hasTutorialGroup);
          await Promise.all(
            [TUTORIAL_BOOK, TUTORIAL_CHAT, TUTORIAL_LINKS].map(graph => props.api.graph.joinGraph(TUTORIAL_HOST, graph)));

          await waiter((p) => {
            return `/ship/${TUTORIAL_HOST}/${TUTORIAL_CHAT}` in p.associations.graph &&
              `/ship/${TUTORIAL_HOST}/${TUTORIAL_BOOK}` in p.associations.graph &&
              `/ship/${TUTORIAL_HOST}/${TUTORIAL_LINKS}` in p.associations.graph;
          });
        }
        nextTutStep();
        dismiss();
      };
      return exitingTut ? (
        <Col maxWidth="350px" p={3}>
          <Icon icon="Info" fill="black"></Icon>
          <Text my={3} lineHeight="tall">
            You can always restart the tutorial by typing &ldquo;tutorial&rdquo; in Leap
          </Text>
          <Row gapX={2} justifyContent="flex-end">
            <Button primary onClick={onDismiss}>Ok</Button>
          </Row>
        </Col>
      ) : (
        <Col maxWidth="350px" p={3}>
          <Box position="absolute" left="-16px" top="-16px">
            <StarIcon width="32px" height="32px" color="blue" display="block" />
          </Box>
          <Text mb={3} lineHeight="tall" fontWeight="medium">Welcome</Text>
          <Text mb={3} lineHeight="tall">
                        You have been invited to use Landscape, an interface to chat
            and interact with communities
            <br />
            Would you like a tour of Landscape?
          </Text>
          <Row gapX={2} justifyContent="flex-end">
            <Button
              backgroundColor="washedGray"
              onClick={() => setExitingTut(true)}
            >Skip</Button>
            <StatelessAsyncButton primary onClick={onContinue}>
              Yes
            </StatelessAsyncButton>
          </Row>
        </Col>
      );
    }
  });

  useEffect(() => {
    if(query.get('tutorial')) {
      if (hasTutorialGroup({ associations })) {
        if (nextTutStep) {
          nextTutStep();
        }
      } else {
        showModal();
      }
    }
  }, [query, showModal]);

  useEffect(() => {
    if(hasLoaded && !seen && tutorialProgress === 'hidden') {
      showModal();
    }
  }, [seen, hasLoaded]);

  return (
    <>
      <Helmet defer={false}>
        <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Landscape</title>
      </Helmet>
      <ScrollbarLessBox height='100%' overflowY='scroll' display="flex" flexDirection="column">
        {modal}
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
          <Tiles
            api={props.api}
          />
          <ModalButton
            icon="Plus"
            bg="washedGray"
            color="black"
            text="New Group"
            style={{ gridColumnStart: 1 }}
          >
            <NewGroup {...props} />
          </ModalButton>
          <ModalButton
            icon="BootNode"
            bg="washedGray"
            color="black"
            text="Join Group"
          >
            <JoinGroup {...props} />
          </ModalButton>
          </>}
          {!hideGroups &&
            (<Groups />)
          }
        </Box>
        <Box alignSelf="flex-start" display={['block', 'none']}>{hashBox}</Box>
      </ScrollbarLessBox>
      <Box display={['none', 'block']}>{hashBox}</Box>
    </>
  );
};

export default LaunchApp;
