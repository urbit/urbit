import React, { useState, useMemo, useEffect } from 'react';
import styled from 'styled-components';
import f from 'lodash/fp';
import _ from 'lodash';

import { Col, Button, Box, Row, Icon, Text } from '@tlon/indigo-react';

import './css/custom.css';
import useContactState from '~/logic/state/contact';
import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { StarIcon } from '~/views/components/StarIcon';
import { writeText } from '~/logic/lib/util';
import { useModal } from "~/logic/lib/useModal";
import { NewGroup } from "~/views/landscape/components/NewGroup";
import { JoinGroup } from "~/views/landscape/components/JoinGroup";
import { Helmet } from 'react-helmet';
import useLocalState from "~/logic/state/local";
import useHarkState from '~/logic/state/hark';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { useQuery } from "~/logic/lib/useQuery";
import {
  hasTutorialGroup,
  TUTORIAL_GROUP,
  TUTORIAL_HOST,
  TUTORIAL_BOOK,
  TUTORIAL_CHAT,
  TUTORIAL_LINKS
} from '~/logic/lib/tutorialModal';
import useLaunchState from '~/logic/state/launch';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import useMetadataState from '~/logic/state/metadata';


const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

const tutSelector = f.pick(['tutorialProgress', 'nextTutStep', 'hideGroups']);

export default function LaunchApp(props) {
  const baseHash = useLaunchState(state => state.baseHash);
  const [hashText, setHashText] = useState(baseHash);
  const [exitingTut, setExitingTut] = useState(false);
  const associations = useMetadataState(s => s.associations);
  const hashBox = (
    <Box
      position={["relative", "absolute"]}
      left="0"
      bottom="0"
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
      <Box backgroundColor="washedGray" p={2}>
        <Text mono bold>{hashText || baseHash}</Text>
      </Box>
    </Box>
  );

  const { query } = useQuery();

  useEffect(() => {
    if(query.get('tutorial')) {
      if(hasTutorialGroup({ associations })) {
        nextTutStep();
      } else {
        showModal();
      }
    }
  }, [query]);

  const calmState = useSettingsState(selectCalmState);
  const { hideUtilities } = calmState;
  const { tutorialProgress, nextTutStep } = useLocalState(tutSelector);
  let { hideGroups } = useLocalState(tutSelector);
  !hideGroups ? { hideGroups } = calmState : null;

  const waiter = useWaitForProps({ ...props, associations });

  const { modal, showModal } = useModal({
    position: 'relative',
    maxWidth: '350px',
    modal: (dismiss) => {
      const onDismiss = (e) => {
        e.stopPropagation();
        props.api.settings.putEntry('tutorial', 'seen', true);
        dismiss();
      };
      const onContinue = async (e) => {
        e.stopPropagation();
        if(!hasTutorialGroup({ associations })) {
          await props.api.groups.join(TUTORIAL_HOST, TUTORIAL_GROUP);
          await props.api.settings.putEntry('tutorial', 'joined', Date.now());
          await waiter(hasTutorialGroup);
          await Promise.all(
            [TUTORIAL_BOOK, TUTORIAL_CHAT, TUTORIAL_LINKS].map(graph =>
              props.api.graph.joinGraph(TUTORIAL_HOST, graph)));

          await waiter(p => {
            return `/ship/${TUTORIAL_HOST}/${TUTORIAL_CHAT}` in p.associations.graph &&
                  `/ship/${TUTORIAL_HOST}/${TUTORIAL_BOOK}` in p.associations.graph &&
                  `/ship/${TUTORIAL_HOST}/${TUTORIAL_LINKS}` in p.associations.graph;
          });
        }
        nextTutStep();
        dismiss();
      }
      return exitingTut ?  (
        <Col maxWidth="350px" p="3">
          <Icon icon="Info" fill="black"></Icon>
          <Text my="3" lineHeight="tall">
            You can always restart the tutorial by typing "tutorial" in Leap
          </Text>
          <Row gapX="2" justifyContent="flex-end">
             <Button primary onClick={onDismiss}>Ok</Button>
          </Row>
        </Col>
      ) : (
        <Col maxWidth="350px" p="3">
          <Box position="absolute" left="-16px" top="-16px">
            <StarIcon width="32px" height="32px" color="blue" display="block" />
          </Box>
          <Text mb="3" lineHeight="tall" fontWeight="medium">Welcome</Text>
          <Text mb="3" lineHeight="tall">
            You have been invited to use Landscape, an interface to chat 
            and interact with communities
            <br />
            Would you like a tour of Landscape?
          </Text>
          <Row gapX="2" justifyContent="flex-end">
            <Button
              backgroundColor="washedGray" 
              onClick={() => setExitingTut(true)}
            >Skip</Button>
            <StatelessAsyncButton primary onClick={onContinue}>
              Yes
            </StatelessAsyncButton>
          </Row>
        </Col>
      )}
  });
  const contacts = useContactState(state => state.contacts);
  const hasLoaded = useMemo(() => Object.keys(contacts).length > 0, [contacts]);

  const notificationsCount = useHarkState(state => state.notificationsCount);

  useEffect(() => {
    const seenTutorial = _.get(props.settings, ['tutorial', 'seen'], true);
    if(hasLoaded && !seenTutorial && tutorialProgress === 'hidden') {
      showModal();
    }
  }, [props.settings, hasLoaded]);

  return (
    <>
      <Helmet defer={false}>
        <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Landscape</title>
      </Helmet>
      <ScrollbarLessBox height='100%' overflowY='scroll' display="flex" flexDirection="column">
        {modal}
        <Box
          mx='2'
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
            <Box p={2} height='100%' width='100%' bg='scales.black20'>
              <Row alignItems='center'>
                <Icon
                  color="black"
                  icon="Home"
                />
                <Text ml="2" mt='1px' color="black">My Channels</Text>
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
            icon="Boot"
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
        <Box alignSelf="flex-start" display={["block", "none"]}>{hashBox}</Box>
      </ScrollbarLessBox>
      <Box display={["none", "block"]}>{hashBox}</Box>
    </>
  );
}
