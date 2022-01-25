import {
  NotificationGraphConfig,
  Unreads
} from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import _ from 'lodash';
import f from 'lodash/fp';
import { emptyHarkStats } from '../state/hark';

export function getLastSeen(
  unreads: Unreads,
  path: string,
  index: string
): BigInteger | undefined {
  const lastSeenIdx = unreads.graph?.[path]?.[index]?.unreads;
  if (!(typeof lastSeenIdx === 'string')) {
    return bigInt.zero;
  }
  return f.flow(f.split('/'), f.last, x => (x ? bigInt(x) : undefined))(
    lastSeenIdx
  );
}

export function getHarkStats(unreads: Unreads, path: string) {
  return unreads?.[path] ?? emptyHarkStats();
}

export function getUnreadCount(
  unreads: Unreads,
  path: string
): number {
  const { count, each } = getHarkStats(unreads, path);
  return count + each.length;
}

export function getNotificationCount(unreads: Unreads, path: string): number {
  const unread = unreads.graph?.[path] || {};
  return Object.keys(unread)
    .map(index => _.get(unread[index], 'notifications.length', 0))
    .reduce(f.add, 0);
}

export function isWatching(
  config: NotificationGraphConfig,
  graph: string,
  index = '/'
) {
  return Boolean(
    config.watching.find(
      watch => watch.graph === graph && watch.index === index
    )
  );
}
