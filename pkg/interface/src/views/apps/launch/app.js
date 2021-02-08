import React, { useState, useEffect } from 'react';
import styled from 'styled-components';
import { useHistory } from 'react-router-dom';
import f from 'lodash/fp';

import { Col, Button, Box, Row, Icon, Text } from '@tlon/indigo-react';

import './css/custom.css';

import Tiles from './components/tiles';
import Tile from './components/tiles/tile';
import Welcome from './components/welcome';
import Groups from './components/Groups';
import ModalButton from './components/ModalButton';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { writeText } from '~/logic/lib/util';
import { useModal } from "~/logic/lib/useModal";
import { NewGroup } from "~/views/landscape/components/NewGroup";
import { JoinGroup } from "~/views/landscape/components/JoinGroup";
import { Helmet } from 'react-helmet';
import useLocalState from "~/logic/state/local";
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { 
  hasTutorialGroup,
  TUTORIAL_GROUP,
  TUTORIAL_HOST
} from '~/logic/lib/tutorialModal';

const ScrollbarLessBox = styled(Box)`
  scrollbar-width: none !important;

  ::-webkit-scrollbar {
    display: none;
  }
`;

const tutSelector = f.pick(['tutorialProgress', 'nextTutStep']);

export default function LaunchApp(props) {
  const history = useHistory();
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

  const { tutorialProgress, nextTutStep } = useLocalState(tutSelector);

  const waiter = useWaitForProps(props);

  const { modal, showModal } = useModal({
    modal: (dismiss) => {
      const onDismiss = (e) => {
        e.stopPropagation();
        props.api.settings.putEntry('tutorial', 'seen', true);
        dismiss();
      };
      const onContinue = async (e) => {
        e.stopPropagation();
        if(!hasTutorialGroup(props)) {
          await props.api.groups.join(TUTORIAL_HOST, TUTORIAL_GROUP);
          await waiter(hasTutorialGroup);
        }
        nextTutStep();
        dismiss();
      }
      return (
      <Col gapY="2" p="3">
        <Text lineHeight="tall" fontWeight="medium">Welcome</Text>
        <Text lineHeight="tall">
          You have been invited to use Landscape, an interface to chat 
          and interact with communities
          <br />
          Would you like a tour of Landscape?
        </Text>
        <Row gapX="2" justifyContent="flex-end">
          <Button onClick={onDismiss}>Skip</Button>
          <StatelessAsyncButton primary onClick={onContinue}>
            Yes
          </StatelessAsyncButton>
        </Row>
      </Col>

    )}
  });
  useEffect(() => {
    const seenTutorial = _.get(props.settings, ['tutorial', 'seen'], true);
    if(!seenTutorial && tutorialProgress === 'hidden') {
      showModal();
    }
  }, [props.settings]);

  return (
    <>
      <Helmet defer={false}>
        <title>{ props.notificationsCount ? `(${String(props.notificationsCount) }) `: '' }Landscape</title>
      </Helmet>
      <ScrollbarLessBox height='100%' overflowY='scroll' display="flex" flexDirection="column">
        {modal}
        <Welcome firstTime={props.launch.firstTime} api={props.api} />
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
            api={props.api}
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
          <Groups unreads={props.unreads} groups={props.groups} associations={props.associations} />
        </Box>
        <Box alignSelf="flex-start" display={["block", "none"]}>{hashBox}</Box>
      </ScrollbarLessBox>
      <Box display={["none", "block"]}>{hashBox}</Box>
    </>
  );
}
