import React, { ReactNode, useCallback } from 'react';
import _ from 'lodash';
import { BigInteger } from 'big-integer';

import { Row, Box } from '@tlon/indigo-react';
import {
  GraphNotificationContents,
  IndexedNotification,
  GroupNotificationContents,
  NotificationGraphConfig,
  GroupNotificationsConfig,
  getParentIndex,
  archive,
  unmute,
  mute
} from '@urbit/api';

import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { GroupNotification } from './group';
import { GraphNotification } from './graph';
import { useHovering } from '~/logic/lib/util';
import useApi from '~/logic/lib/useApi';

interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  archived: boolean;
  graphConfig: NotificationGraphConfig;
  groupConfig: GroupNotificationsConfig;
}

function getMuted(
  idxNotif: IndexedNotification,
  groups: GroupNotificationsConfig,
  graphs: NotificationGraphConfig
) {
  const { index, notification } = idxNotif;
  if ('graph' in idxNotif.index) {
    const { graph } = idxNotif.index.graph;
    if(!('graph' in notification.contents)) {
      throw new Error();
    }
    const parent = getParentIndex(index.graph, notification.contents.graph);

    return _.findIndex(
      graphs?.watching || [],
      g => g.graph === graph && g.index === parent
    ) === -1;
  }
  if ('group' in index) {
    return _.findIndex(groups || [], g => g === index.group.group) === -1;
  }
  return false;
}

function NotificationWrapper(props: {
  time: BigInteger;
  notif: IndexedNotification;
  children: ReactNode;
  archived: boolean;
  graphConfig: NotificationGraphConfig;
  groupConfig: GroupNotificationsConfig;
}) {
  const { time, notif, children } = props;
  const api = useApi();

  const onArchive = useCallback(async () => {
    api.poke(archive(time, notif.index));
  }, [time, notif]);

  const isMuted = getMuted(
    notif,
    props.groupConfig,
    props.graphConfig
  );

  const onChangeMute = useCallback(async () => {
    if (isMuted) {
      api.poke(unmute(notif));
    } else {
      api.poke(mute(notif));
    }
  }, [notif, isMuted]);

  const { hovering, bind } = useHovering();

  const changeMuteDesc = isMuted ? 'Unmute' : 'Mute';
  return (
    <Box
      width="100%"
      display="grid"
      gridTemplateColumns="1fr 200px"
      gridTemplateRows="auto"
      gridTemplateAreas="'header actions' 'main main'"
      pb={2}
      {...bind}
    >
      {children}
      <Row gapX="2" p="2" pt='3' gridArea="actions" justifyContent="flex-end" opacity={[1, hovering ? 1 : 0]}>
        <StatelessAsyncAction name={changeMuteDesc} onClick={onChangeMute} backgroundColor="transparent">
          {changeMuteDesc}
        </StatelessAsyncAction>
        {!props.archived && (
          <StatelessAsyncAction name={time.toString()} onClick={onArchive} backgroundColor="transparent">
            Dismiss
          </StatelessAsyncAction>
        )}
      </Row>
    </Box>
  );
}

export function Notification(props: NotificationProps) {
  const { notification, archived } = props;
  const { read, contents, time } = notification.notification;

  const Wrapper = ({ children }) => (
    <NotificationWrapper
      archived={archived}
      notif={notification}
      time={props.time}
      graphConfig={props.graphConfig}
      groupConfig={props.groupConfig}
    >
      {children}
    </NotificationWrapper>
  );

  if ('graph' in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <Wrapper>
        <GraphNotification
          index={index}
          contents={c}
          read={read}
          archived={archived}
          timebox={props.time}
          time={time}
        />
      </Wrapper>
    );
  }
  if ('group' in notification.index) {
    const index = notification.index.group;
    const c: GroupNotificationContents = (contents as any).group;
    return (
      <Wrapper>
        <GroupNotification
          index={index}
          contents={c}
          read={read}
          timebox={props.time}
          archived={archived}
          time={time}
        />
      </Wrapper>
    );
  }

  return null;
}
