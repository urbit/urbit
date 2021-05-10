import { Box, Center, Col, LoadingSpinner, Text } from '@tlon/indigo-react';
import {
  IndexedNotification,

  JoinRequests, Notifications,

  Timebox
} from '@urbit/api';
import { BigInteger } from 'big-integer';
import _ from 'lodash';
import f from 'lodash/fp';
import moment from 'moment';
import React, { useCallback, useEffect, useRef } from 'react';
import GlobalApi from '~/logic/api/global';
import { getNotificationKey } from '~/logic/lib/hark';
import { useLazyScroll } from '~/logic/lib/useLazyScroll';
import { daToUnix, MOMENT_CALENDAR_DATE } from '~/logic/lib/util';
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

  const ready = useHarkState(
    s => Object.keys(s.unreads.graph).length > 0
  );

  const notificationState = useHarkState(state => state.notifications);
  const archivedNotifications = useHarkState(state => state.archivedNotifications);

  const notifications =
    Array.from(props.showArchive ? archivedNotifications : notificationState) || [];

  const calendar = {
    ...MOMENT_CALENDAR_DATE, sameDay: function (now) {
      if (this.subtract(6, 'hours').isBefore(now)) {
        return '[Earlier Today]';
      } else {
        return MOMENT_CALENDAR_DATE.sameDay;
      }
    }
  };

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

  return (
    <Col p="1" ref={scrollRef} position="relative" height="100%" overflowY="auto" overflowX="hidden">
      <Invites pendingJoin={props.pendingJoin} api={api} />
      {[...notificationsByDayMap.keys()].sort().reverse().map((day, index) => {
        const timeboxes = notificationsByDayMap.get(day)!;
        return timeboxes.length > 0 && (
          <DaySection
            key={day}
            time={day}
            label={day === 'latest' ? 'Today' : moment(day).calendar(null, calendar)}
            timeboxes={timeboxes}
            archive={Boolean(props.showArchive)}
            api={api}
          />
        );
      })}
      {isDone ? (
        <Center mt="2" borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="lightGray" width="100%" height="96px">
          <Text gray fontSize="1">No more notifications</Text>
        </Center>
    )  : isLoading ? (
        <Center mt="2" borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="lightGray" width="100%" height="96px">
          <LoadingSpinner />
        </Center>
    ) : (
      <Box mt="2" height="96px" />
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
  label,
  archive,
  timeboxes,
  time,
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
            archived={archive}
            time={date}
          />
        ))
      )}
    </>
  );
}
