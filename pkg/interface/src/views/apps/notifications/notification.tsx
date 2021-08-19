import { Box, Button, Icon, Row } from '@tlon/indigo-react';
import {
  GraphNotificationContents,

  GroupNotificationContents,

  GroupNotificationsConfig, IndexedNotification,

  NotificationGraphConfig
} from '@urbit/api';
import { BigInteger } from 'big-integer';
import _ from 'lodash';
import React, { ReactNode, useCallback } from 'react';
import GlobalApi from '~/logic/api/global';
import { getNotificationKey } from '~/logic/lib/hark';
import { getParentIndex } from '~/logic/lib/notification';
import { useHovering } from '~/logic/lib/util';
import useHarkState from '~/logic/state/hark';
import useLocalState from '~/logic/state/local';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { SwipeMenu } from '~/views/components/SwipeMenu';
import { GraphNotification } from './graph';
import { GroupNotification } from './group';

export interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  api: GlobalApi;
  unread: boolean;
}

function getMuted(
  idxNotif: IndexedNotification,
  groups: GroupNotificationsConfig,
  graphs: NotificationGraphConfig
) {
  const { index, notification } = idxNotif;
  if ('graph' in idxNotif.index) {
    const { graph } = idxNotif.index.graph;
    if (!('graph' in notification.contents)) {
      throw new Error();
    }
    const parent = getParentIndex(idxNotif.index.graph, notification.contents.graph);

    return (
      _.findIndex(
        graphs?.watching || [],
        g => g.graph === graph && g.index === parent
      ) === -1
    );
  }
  if ('group' in index) {
    return _.findIndex(groups || [], g => g === index.group.group) === -1;
  }
  return false;
}

export function NotificationWrapper(props: {
  api: GlobalApi;
  time?: BigInteger;
  read?: boolean;
  notification?: IndexedNotification;
  children: ReactNode;
}) {
  const { api, time, notification, children, read = false } = props;

  const isMobile = useLocalState(s => s.mobile);

  const onArchive = useCallback(async (e) => {
    e.stopPropagation();
    if (!notification) {
      return;
    }
    return api.hark.archive(time, notification.index);
  }, [time, notification]);

  const groupConfig = useHarkState(state => state.notificationsGroupConfig);
  const graphConfig = useHarkState(state => state.notificationsGraphConfig);

  const isMuted =
    time && notification && getMuted(notification, groupConfig, graphConfig);

  const onClick = (e: any) => {
    if (!notification || read) {
      return;
    }
    return api.hark.read(time, notification.index);
  };

  const { hovering, bind } = useHovering();

  return (
    <SwipeMenu
      key={(time && notification && getNotificationKey(time, notification)) ?? 'unknown'}
      m={2}
      menuWidth={100}
      disabled={!isMobile}
      menu={
        <Button onClick={onArchive} ml={2} height="100%" width="92px" primary destructive>
          Remove
        </Button>
      }
    >
      <Box
        onClick={onClick}
        bg={read ? 'washedGray' : 'washedBlue'}
        borderRadius={2}
        display="grid"
        gridTemplateColumns={['1fr 24px', '1fr 200px']}
        gridTemplateRows="auto"
        gridTemplateAreas="'header actions' 'main main'"
        p={2}
        {...bind}
      >
        {children}
        <Row
          alignItems="flex-start"
          gapX={2}
          gridArea="actions"
          justifyContent="flex-end"
          opacity={[0, hovering ? 1 : 0]}
        >
          {notification && (
            <StatelessAsyncAction
              name=""
              borderRadius={1}
              onClick={onArchive}
              backgroundColor="white"
            >
              <Icon lineHeight="24px" size={16} icon="X" />
            </StatelessAsyncAction>
          )}
        </Row>
      </Box>
    </SwipeMenu>
  );
}

export function Notification(props: NotificationProps) {
  const { notification, unread } = props;
  const { contents, time } = notification.notification;

  const wrapperProps = {
    notification,
    read: !unread,
    time: props.time,
    api: props.api
  };

  if ('graph' in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <NotificationWrapper {...wrapperProps}>
        <GraphNotification
          api={props.api}
          index={index}
          contents={c}
          read={!unread}
          timebox={props.time}
          time={time}
        />
      </NotificationWrapper>
    );
  }
  if ('group' in notification.index) {
    const index = notification.index.group;
    const c: GroupNotificationContents = (contents as any).group;
    return (
      <NotificationWrapper {...wrapperProps}>
        <GroupNotification
          api={props.api}
          index={index}
          contents={c}
          read={!unread}
          timebox={props.time}
          time={time}
        />
      </NotificationWrapper>
    );
  }

  return null;
}
