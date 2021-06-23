import { Box, Center, Col, LoadingSpinner, Text, Icon } from '@tlon/indigo-react';
import {
    IndexedNotification,

    JoinRequests, Notifications,

    Timebox,
    unixToDa
} from '@urbit/api';
import { BigInteger } from 'big-integer';
import _ from 'lodash';
import f from 'lodash/fp';
import moment from 'moment';
import React, { useCallback, useEffect, useRef } from 'react';
import GlobalApi from '~/logic/api/global';
import { getNotificationKey } from '~/logic/lib/hark';
import { useLazyScroll } from '~/logic/lib/useLazyScroll';
import useLaunchState from '~/logic/state/launch';
import { daToUnix } from '~/logic/lib/util';
import useHarkState from '~/logic/state/hark';
import { Invites } from './invites';
import { Notification } from './notification';

type DatedTimebox = [BigInteger, Timebox];

function filterNotification(groups: string[]) {
  if (groups.length === 0) {
    return () => true;
  }
  return (n: IndexedNotification) => {
    if ('graph' in n.index) {
      const { group } = n.index.graph;
      return groups.findIndex(g => group === g) !== -1;
    } else if ('group' in n.index) {
      const { group } = n.index.group;
      return groups.findIndex(g => group === g) !== -1;
    }
    return true;
  };
}

export default function Inbox(props: {
  archive: Notifications;
  showArchive?: boolean;
  api: GlobalApi;
  filter: string[];
  pendingJoin: JoinRequests;
}) {
  const { api } = props;
  useEffect(() => {
    let seen = false;
    setTimeout(() => {
      seen = true;
    }, 3000);
    return () => {
      if (seen) {
        api.hark.seen();
      }
    };
  }, []);

  const runtimeLag = useLaunchState(state => state.runtimeLag);

  const ready = useHarkState(
    s => Object.keys(s.unreads.graph).length > 0
  );

  const notificationState = useHarkState(state => state.notifications);
  const unreadNotes = useHarkState(s => s.unreadNotes);
  const archivedNotifications = useHarkState(state => state.archivedNotifications);

  const notifications =
    Array.from(props.showArchive ? archivedNotifications : notificationState) || [];

  const notificationsByDay = f.flow(
    f.map<DatedTimebox, DatedTimebox>(([date, nots]) => [
      date,
      nots.filter(filterNotification(props.filter))
    ]),
    f.groupBy<DatedTimebox>(([d]) => {
      const date = moment(daToUnix(d));
      if (moment().subtract(6, 'hours').isBefore(date)) {
        return 'latest';
      } else {
        return date.format('YYYYMMDD');
      }
    })
  )(notifications);

  const notificationsByDayMap = new Map<string, DatedTimebox[]>(
    Object.keys(notificationsByDay).map((timebox) => {
      return [timebox, notificationsByDay[timebox]];
    })
  );

  const scrollRef = useRef(null);

  const loadMore = useCallback(async () => {
    return api.hark.getMore();
  }, [api]);

  const { isDone, isLoading } = useLazyScroll(
    scrollRef,
    ready,
    0.2,
    _.flatten(notifications).length,
    loadMore
  );
  const date = unixToDa(Date.now());

  return (
    <Col p={1} ref={scrollRef} position="relative" height="100%" overflowY="auto" overflowX="hidden">
      {runtimeLag && (
        <Box bg="yellow" borderRadius={2} p={2} m={2}>
          <Icon verticalAlign="middle" mr={2} icon="Tutorial" />
          <Text verticalAlign="middle">
            Update your binary to continue receiving updates.
          </Text>
        </Box>
      )}
      <Invites pendingJoin={props.pendingJoin} api={api} />
      <DaySection unread key="unread" timeboxes={[[date,unreadNotes]]} api={api} />
      {[...notificationsByDayMap.keys()].sort().reverse().map((day, index) => {
        const timeboxes = notificationsByDayMap.get(day)!;
        return timeboxes.length > 0 && (
          <DaySection
            key={day}
            timeboxes={timeboxes}
            api={api}
          />
        );
      })}
      {isDone ? (
        <Center mt={2} borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="lightGray" width="100%" height="96px">
          <Text gray fontSize={1}>No more notifications</Text>
        </Center>
    )  : isLoading ? (
        <Center mt={2} borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="lightGray" width="100%" height="96px">
          <LoadingSpinner />
        </Center>
    ) : (
      <Box mt={2} height="96px" />
    )}

    </Col>
  );
}

function sortTimeboxes([a]: DatedTimebox, [b]: DatedTimebox) {
  return b.subtract(a);
}

function sortIndexedNotification(
  { notification: a }: IndexedNotification,
  { notification: b }: IndexedNotification
) {
  return b.time - a.time;
}

function DaySection({
  timeboxes,
  unread = false,
  api
}) {
  const lent = timeboxes.map(([,nots]) => nots.length).reduce(f.add, 0);
  if (lent === 0 || timeboxes.length === 0) {
    return null;
  }

  return (
    <>
      {_.map(timeboxes.sort(sortTimeboxes), ([date, nots], i: number) =>
        _.map(nots.sort(sortIndexedNotification), (not, j: number) => (
          <Notification
            key={getNotificationKey(date, not)}
            api={api}
            notification={not}
            unread={unread}
            time={!unread ? date : undefined}
          />
        ))
      )}
    </>
  );
}
