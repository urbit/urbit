import React, { useEffect, useCallback, useRef } from 'react';
import f from 'lodash/fp';
import _ from 'lodash';
import moment from 'moment';
import { BigInteger } from 'big-integer';

import { Col, Center, Box, Text, LoadingSpinner } from '@tlon/indigo-react';
import {
  Associations,
  Notifications,
  Rolodex,
  Timebox,
  IndexedNotification,
  Groups,
  JoinRequests,
  GroupNotificationsConfig,
  NotificationGraphConfig,
  Invites as InviteType
} from '@urbit/api';

import { MOMENT_CALENDAR_DATE, daToUnix } from '~/logic/lib/util';
import GlobalApi from '~/logic/api/global';
import { Notification } from './notification';
import { Invites } from './invites';
import { useLazyScroll } from '~/logic/lib/useLazyScroll';
import useHarkState from '~/logic/state/hark';
import useInviteState from '~/logic/state/invite';

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
    0.2,
    _.flatten(notifications).length,
    loadMore
  );

  return (
    <Col ref={scrollRef} position="relative" height="100%" overflowY="auto">
      <Invites pendingJoin={props.pendingJoin} api={api} />
      {[...notificationsByDayMap.keys()].sort().reverse().map((day, index) => {
        const timeboxes = notificationsByDayMap.get(day)!;
        return timeboxes.length > 0 && (
          <DaySection
            key={day}
            label={day === 'latest' ? 'Today' : moment(day).calendar(null, calendar)}
            timeboxes={timeboxes}
            archive={Boolean(props.showArchive)}
            api={api}
          />
        );
      })}
      {isDone && (
        <Center mt="2" borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="washedGray" width="100%" height="96px">
          <Text gray fontSize="1">No more notifications</Text>
        </Center>
    )}
      {isLoading && (
        <Center mt="2" borderTop={notifications.length !== 0 ? 1 : 0} borderTopColor="washedGray" width="100%" height="96px">
          <LoadingSpinner />
        </Center>
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
  api,
}) {
  const lent = timeboxes.map(([,nots]) => nots.length).reduce(f.add, 0);
  if (lent === 0 || timeboxes.length === 0) {
    return null;
  }

  return (
    <>
      <Box position="sticky" zIndex={3} top="-1px" bg="white">
        <Box p="2" bg="scales.black05">
          <Text>
            {label}
          </Text>
        </Box>
      </Box>
      {_.map(timeboxes.sort(sortTimeboxes), ([date, nots], i: number) =>
        _.map(nots.sort(sortIndexedNotification), (not, j: number) => (
          <React.Fragment key={j}>
            {(i !== 0 || j !== 0) && (
              <Box flexShrink={0} height="4px" bg="scales.black05" />
            )}
            <Notification
              api={api}
              notification={not}
              archived={archive}
              time={date}
            />
          </React.Fragment>
        ))
      )}
    </>
  );
}
