import { Col, Text, Row, Action, LoadingSpinner } from '@tlon/indigo-react';
import { Association, Associations, Invite, JoinRequest, Unreads } from '@urbit/api';
import f from 'lodash/fp';
import _ from 'lodash';
import moment from 'moment';
import React, { useRef } from 'react';
import { getNotificationCount, getUnreadCount } from '~/logic/lib/hark';
import { TUTORIAL_GROUP, TUTORIAL_GROUP_RESOURCE, TUTORIAL_HOST } from '~/logic/lib/tutorialModal';
import { alphabeticalOrder } from '~/logic/lib/util';
import useGroupState, { useJoiningGroup } from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useInviteState, { useGroupInvite } from '~/logic/state/invite';
import useMetadataState, { usePreview } from '~/logic/state/metadata';
import useSettingsState, { selectCalmState, SettingsState } from '~/logic/state/settings';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import Tile from '../components/tiles/tile';

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

export default function Groups() {
  const unreads = useHarkState(state => state.unreads);
  const groupState = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);

  const groups = Object.values(associations?.groups || {})
    .filter(e => e?.group in groupState)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || {} as Associations, unreads);
  const graphNotifications = getGraphNotifications(associations || {} as Associations, unreads);

  const invites = useInviteState(s =>
    Object.values(s.invites.groups || {})
      .map(({ resource }) => `/ship/~${resource.ship}/${resource.name}`)
  );
  const joining =  useGroupState(s => Object.keys(s.pendingJoin || {}));

  const pending = _.difference(
    _.uniq([...invites, ...joining].sort(alphabeticalOrder)),
    _.map(groups, 'group')
  );

  return (
    <>
      {groups.map((group, index) => {
        const path = group?.group;
        const unreadCount = graphUnreads(path);
        const notCount = graphNotifications(path);
        return (
          <Group
            key={group?.group}
            updates={notCount}
            first={index === 0}
            unreads={unreadCount}
            path={group?.group}
            title={group.metadata.title}
          />
        );
      })}
      {pending.map((group, i) => (
        <PendingGroup first={i === 0} key={group} group={group} />
      ))}
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
  const anchorRef = useRef<HTMLDivElement>(null);
  const isTutorialGroup = path === `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`;
  useTutorialModal(
    'start',
    isTutorialGroup,
    anchorRef
  );
  const { hideUnreads } = useSettingsState(selectCalmState);
  const joined = useSettingsState(selectJoined);
  const days = Math.max(0, Math.floor(moment.duration(moment(joined)
        .add(14, 'days')
        .diff(moment()))
    .as('days'))) || 0;
  return (
    <Tile ref={anchorRef} position="relative" bg={isTutorialGroup ? 'lightBlue' : undefined} to={`/~landscape${path}`} gridColumnStart={first ? 1 : null}>
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        {!hideUnreads && (<Col>
          {isTutorialGroup && joined &&
            (<Text>{days} day{days !== 1 && 's'} remaining</Text>)
          }
          {updates > 0 &&
            (<Text mt={1} color="blue">{updates} update{updates !== 1 && 's'} </Text>)
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

function PendingGroup({ group, first = false }: { group: string; first?: boolean }) {
  const invite = useGroupInvite(group);
  const pending = useJoiningGroup(group);

  if(pending) {
    return <PendingJoin group={group} request={pending} first={first} />;
  } else if (invite) {
    return <PendingInvite invite={invite} uid={invite.uid} first={first} />;
  } else {
    return null;
  }
}

function PendingJoin(props: { group: string; request: JoinRequest; first: boolean; }) {
  const { request, first, group } = props;
  const { preview } = usePreview(group);
  if(request.progress === 'done') {
    return null;
  }
  return (
    <Tile bg="#555" gridColumnStart={first ? 1 : null}>
      <Col height="100%" justifyContent="space-between">
        {preview?.metadata?.title || group}
        <LoadingSpinner />
      </Col>
    </Tile>
  );
}

function PendingInvite(props: { invite: Invite; uid: string; first: boolean; }) {
  const { invite, uid, first } = props;
  const { resource: { ship, name } } = invite;
  const path = `/ship/~${ship}/${name}`;
  const { preview } = usePreview(path);

  const onAccept = () => {
    useGroupState.getState().joinGroup(path, uid);
  };

  const onDecline = () => {
    useInviteState.getState().declineInvite('groups', uid);
  };

  return (
    <Tile bg="#555" gridColumnStart={first ? 1 : null}>
      <Col height="100%" justifyContent="space-between">
        <Text>{preview?.metadata?.title ?? (invite.resource.ship + '/' +invite.resource.name)}</Text>
      <Row justifyContent="space-between">
        <Action destructive backgroundColor="transparent" onClick={onDecline}>
          Decline
        </Action>
        <Action primary backgroundColor="transparent" onClick={onAccept}>
          Accept
        </Action>
      </Row>
      </Col>
    </Tile>

  );
}
