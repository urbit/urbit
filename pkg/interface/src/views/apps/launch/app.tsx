import React, { useState, useMemo, useEffect } from 'react';
import styled from 'styled-components';
import { Helmet } from 'react-helmet';
import f from 'lodash/fp';
import _ from 'lodash';

import { Col, Button, Box, Row, Icon, Text } from '@tlon/indigo-react';
import { join, joinGraph } from '@urbit/api';

import './css/custom.css';
import Tiles from './components/Tiles';
import Tile from './components/tiles/tile';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { writeText } from '~/logic/lib/util';
import { useModal } from "~/logic/lib/useModal";
import { NewGroup } from "~/views/landscape/components/NewGroup";
import { JoinGroup } from "~/views/landscape/components/JoinGroup";
import useLocalState from "~/logic/state/local";
import useContactState from '~/logic/state/contacts';
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
import useHarkState from '~/logic/state/hark';
import useApi from '~/logic/lib/useApi';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

const tutSelector = f.pick(['tutorialProgress', 'nextTutStep']);

export default function LaunchApp(props) {
  const contacts = useContactState(state => state.contacts);
  const [hashText, setHashText] = useState(props.baseHash);
  const hashBox = (
    <Box
      position={["relative", "absolute"]}
      fontFamily="mono"
      left="0"
      bottom="0"
      color="washedGray"
      bg="white"
      ml={3}
      mb={3}
      borderRadius={2}
      fontSize={0}
      p={2}
      boxShadow="0 0 0px 1px inset"
      cursor="pointer"
      onClick={() => {
        writeText(props.baseHash);
        setHashText('copied');
        setTimeout(() => {
          setHashText(props.baseHash);
        }, 2000);
      }}
    >
      <Text color="gray">{hashText || props.baseHash}</Text>
    </Box>
  );

  const { query } = useQuery();

  useEffect(() => {
    if(query.get('tutorial')) {
      if(hasTutorialGroup(props)) {
        nextTutStep();
      } else {
        showModal();
      }
    }
  }, [query]);

  const { tutorialProgress, nextTutStep } = useLocalState(tutSelector);

  const waiter = useWaitForProps(props);

  const api = useApi();

  const { modal, showModal } = useModal({
    position: 'relative', 
    maxWidth: '350px',
    modal: (dismiss) => {
      const onDismiss = (e) => {
        e.stopPropagation();
        // TODO convert this with settings
        props.api.settings.putEntry('tutorial', 'seen', true);
        dismiss();
      };
      const onContinue = async (e) => {
        e.stopPropagation();
        if (!hasTutorialGroup(props)) {
          await api.poke(join(TUTORIAL_HOST, TUTORIAL_GROUP));

          await waiter(hasTutorialGroup);
          await Promise.all(
            [TUTORIAL_BOOK, TUTORIAL_CHAT, TUTORIAL_LINKS].map(graph =>
              api.poke(joinGraph(TUTORIAL_HOST, graph))));

          await waiter(p => {
            return `/ship/${TUTORIAL_HOST}/${TUTORIAL_CHAT}` in p.associations.graph &&
                  `/ship/${TUTORIAL_HOST}/${TUTORIAL_BOOK}` in p.associations.graph &&
                  `/ship/${TUTORIAL_HOST}/${TUTORIAL_LINKS}` in p.associations.graph;
          });
        }
        nextTutStep();
        dismiss();
      }
      return (
      <Col maxWidth="350px" gapY="2" p="3">
        <Box position="absolute" left="-16px" top="-16px">
          <Icon width="32px" height="32px" color="blue" display="block" icon="LargeBullet" />
        </Box>
        <Text lineHeight="tall" fontWeight="medium">Welcome</Text>
        <Text lineHeight="tall">
          You have been invited to use Landscape, an interface to chat 
          and interact with communities
          <br />
          Would you like a tour of Landscape?
        </Text>
        <Row gapX="2" justifyContent="flex-end">
          <Button backgroundColor="washedGray" onClick={onDismiss}>Skip</Button>
          <StatelessAsyncButton primary onClick={onContinue}>
            Yes
          </StatelessAsyncButton>
        </Row>
      </Col>
    )}
  });
  const hasLoaded = useMemo(() => Object.keys(contacts).length > 0, [contacts]);

  useEffect(() => {
    // TODO Settings store
    const seenTutorial = _.get(props.settings, ['tutorial', 'seen'], true);
    if(hasLoaded && !seenTutorial && tutorialProgress === 'hidden') {
      showModal();
    }
  }, [props.settings, hasLoaded]);

  const notificationsCount = useHarkState(state => state.notificationsCount);

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
            tiles={props.launch.tiles}
            tileOrdering={props.launch.tileOrdering}
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
            text="Create Group"
          >
            <NewGroup {...props} />
          </ModalButton>
          <Groups />
        </Box>
        <Box alignSelf="flex-start" display={["block", "none"]}>{hashBox}</Box>
      </ScrollbarLessBox>
      <Box display={["none", "block"]}>{hashBox}</Box>
    </>
  );
}
