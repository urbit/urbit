import { Box, Button, Icon, Row } from '@tlon/indigo-react';
import {
  archive,
  GraphNotificationContents,
  GroupNotificationContents,
  IndexedNotification,
  readNote
} from '@urbit/api';
import { BigInteger } from 'big-integer';
import React, { ReactNode, useCallback } from 'react';
import { getNotificationKey } from '~/logic/lib/hark';
import { useHovering } from '~/logic/lib/util';
import useLocalState from '~/logic/state/local';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { SwipeMenu } from '~/views/components/SwipeMenu';
import { GraphNotification } from './graph';
import { GroupNotification } from './group';
import airlock from '~/logic/api';

export interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  unread: boolean;
}

export function NotificationWrapper(props: {
  time?: BigInteger;
  read?: boolean;
  notification?: IndexedNotification;
  children: ReactNode;
}) {
  const { time, notification, children, read = false } = props;

  const isMobile = useLocalState(s => s.mobile);

  const onArchive = useCallback(async (e) => {
    e.stopPropagation();
    if (!notification) {
      return;
    }
    await airlock.poke(archive(notification.index, time));
  }, [time, notification]);

  const onClick = (e: any) => {
    if (!notification || read) {
      return;
    }
    return airlock.poke(readNote(notification.index));
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
    time: props.time
  };

  if ('graph' in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <NotificationWrapper {...wrapperProps}>
        <GraphNotification
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
          index={index}
          contents={c}
          timebox={props.time}
          time={time}
        />
      </NotificationWrapper>
    );
  }

  return null;
}
