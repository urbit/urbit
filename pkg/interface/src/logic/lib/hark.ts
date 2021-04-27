import bigInt, { BigInteger } from 'big-integer';
import f from 'lodash/fp';
import { Unreads } from '@urbit/api';

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

export function getUnreadCount(
  unreads: Unreads,
  path: string,
  index: string
): number {
  const graphUnreads = unreads.graph?.[path]?.[index]?.unreads ?? 0;
  return typeof graphUnreads === 'number' ? graphUnreads : graphUnreads.size;
}

export function getNotificationCount(
  unreads: Unreads,
  path: string
): number {
  const unread = unreads.graph?.[path] || {};
  return Object.keys(unread)
    .map(index => unread[index]?.notifications?.length || 0)
    .reduce(f.add, 0);
}
