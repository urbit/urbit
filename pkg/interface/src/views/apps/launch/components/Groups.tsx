import React, { useRef } from 'react';
import { Box, Text, Col } from '@tlon/indigo-react';
import f from 'lodash/fp';
import _ from 'lodash';
import moment from 'moment';

import { Associations, Association, Unreads, UnreadStats } from '@urbit/api';
import { alphabeticalOrder } from '~/logic/lib/util';
import { getUnreadCount, getNotificationCount } from '~/logic/lib/hark';
import Tile from '../components/tiles/tile';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import { TUTORIAL_HOST, TUTORIAL_GROUP, TUTORIAL_GROUP_RESOURCE } from '~/logic/lib/tutorialModal';
import useSettingsState, { selectCalmState, SettingsState } from '~/logic/state/settings';

interface GroupsProps {}

const sortGroupsAlph = (a: Association, b: Association) =>
  a.group === TUTORIAL_GROUP_RESOURCE
    ? -1
    : b.group === TUTORIAL_GROUP_RESOURCE
    ? 1
    : alphabeticalOrder(a.metadata.title, b.metadata.title);

const getGraphUnreads = (associations: Associations, unreads: Unreads) => (path: string) =>
  f.flow(
    f.pickBy((a: Association) => a.group === path),
    f.map('resource'),
    f.map(rid => getUnreadCount(unreads, rid, '/')),
    f.reduce(f.add, 0)
  )(associations.graph);

const getGraphNotifications = (associations: Associations, unreads: Unreads) => (path: string) =>
  f.flow(
    f.pickBy((a: Association) => a.group === path),
    f.map('resource'),
    f.map(rid => getNotificationCount(unreads, rid)),
    f.reduce(f.add, 0)
  )(associations.graph);

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { inbox, ...boxProps } = props;
  const unreads = useHarkState(state => state.unreads);
  const groupState = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);

  const groups = Object.values(associations?.groups || {})
    .filter(e => e?.group in groupState)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || {}, unreads);
  const graphNotifications = getGraphNotifications(associations || {}, unreads);

  return (
    <>
      {groups.map((group, index) => {
        const path = group?.group;
        const unreadCount = graphUnreads(path);
        const notCount = graphNotifications(path);

        return (
          <Group
            updates={notCount}
            first={index === 0}
            unreads={unreadCount}
            path={group?.group}
            title={group.metadata.title}
            picture={group.metadata.picture}
          />
        );
      })}
    </>
  );
}

interface GroupProps {
  path: string;
  title: string;
  updates: number;
  unreads: number;
  first: boolean;
}
const selectJoined = (s: SettingsState) => s.tutorial.joined;
function Group(props: GroupProps) {
  const { path, title, unreads, updates, first = false } = props;
  const anchorRef = useRef<HTMLElement>(null);
  const isTutorialGroup = path === `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`;
  useTutorialModal(
    'start',
    isTutorialGroup,
    anchorRef
  );
  const { hideUnreads } = useSettingsState(selectCalmState)
  const joined = useSettingsState(selectJoined);
  return (
    <Tile ref={anchorRef} position="relative" bg={isTutorialGroup ? 'lightBlue' : undefined} to={`/~landscape${path}`} gridColumnStart={first ? '1' : null}>
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        {!hideUnreads && (<Col>
          {isTutorialGroup && joined && 
            (<Text>{Math.floor(moment.duration(moment(joined).add(14, 'days').diff(moment())).as('days'))} days remaining</Text>)
          }
          {updates > 0 &&
            (<Text mt="1" color="blue">{updates} update{updates !== 1 && 's'} </Text>)
          }
          {unreads > 0 &&
            (<Text color="lightGray">{unreads}</Text>)
          }
        </Col>
        )}
      </Col>
    </Tile>
  );
}
