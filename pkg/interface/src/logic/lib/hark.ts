import { GraphNotifIndex, GroupNotifIndex, IndexedNotification, NotificationGraphConfig, Post, Unreads } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import _ from 'lodash';
import f from 'lodash/fp';
import { pluralize } from './util';

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

export function isWatching(
  config: NotificationGraphConfig,
  graph: string,
  index = '/'
) {
  return Boolean(config.watching.find(
    watch => watch.graph === graph && watch.index === index
  ));
}

export function getNotificationKey(time: BigInteger, notification: IndexedNotification): string {
  const base = time.toString();
  if('graph' in notification.index) {
    const { graph, index } = notification.index.graph;
    return `${base}-${graph}-${index}`;
  } else if('group' in notification.index) {
    const { group } = notification.index.group;
    return `${base}-${group}`;
  }
  return `${base}-unknown`;
}


export function notificationReferent(not: IndexedNotification) {
  if('graph' in not.index) {
    return not.index.graph.graph;
  } else {
    return not.index.group.group;
  }
}
export function describeNotification(notification: IndexedNotification) {
  function group(idx: GroupNotifIndex) {
    switch (idx.description) {
      case 'add-members':
        return 'joined';
      case 'remove-members':
        return 'left';
      default:
        return idx.description;
    }
  }
  function graph(idx: GraphNotifIndex, plural: boolean, singleAuthor: boolean) {
    const isDm = idx.graph.startsWith('dm--');
    switch (idx.description) {
      case 'post':
        return singleAuthor ? 'replied to you' : 'Your post received replies';
      case 'link':
        return `New link${plural ? 's' : ''} in`;
      case 'comment':
        return `New comment${plural ? 's' : ''} on`;
      case 'note':
        return `New Note${plural ? 's' : ''} in`;
      case 'edit-note':
        return `updated ${pluralize('note', plural)} in`;
      case 'mention':
        return singleAuthor ? 'mentioned you in' : 'You were mentioned in';
      case 'message':
        if (isDm) {
          return 'messaged you';
        }
        return `New message${plural ? 's' : ''} in`;
      default:
        return idx.description;
    }
  }
  if('group' in notification.index) {
    return group(notification.index.group);
  } else if('graph' in notification.index) {
    const contents = notification.notification?.contents?.graph ?? [] as Post[];
    return graph(notification.index.graph, contents.length > 1, _.uniq(_.map(contents, 'author')).length === 1)

  }
}
