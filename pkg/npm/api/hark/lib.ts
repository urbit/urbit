import f from 'lodash/fp';
import bigInt, { BigInteger } from 'big-integer';

import { Poke } from '../lib/types';
import { GraphNotifDescription, GraphNotificationContents, GraphNotifIndex, IndexedNotification, NotifIndex, Unreads } from './types';
import { decToUd } from '../lib';
import { Association } from '../metadata/types';

export const harkAction = <T>(data: T): Poke<T> => ({
  app: 'hark-store',
  mark: 'hark-action',
  json: data
});

const graphHookAction = <T>(data: T): Poke<T> => ({
  app: 'hark-graph-hook',
  mark: 'hark-graph-hook-action',
  json: data
});

export { graphHookAction as harkGraphHookAction };

const groupHookAction = <T>(data: T): Poke<T> => ({
  app: 'hark-group-hook',
  mark: 'hark-group-hook-action',
  json: data
});

export { groupHookAction as harkGroupHookAction };

export const actOnNotification = (
  frond: string,
  intTime: BigInteger,
  index: NotifIndex
): Poke<unknown> => harkAction({
  [frond]: {
    time: decToUd(intTime.toString()),
    index
  }
});

export const getParentIndex = (
  idx: GraphNotifIndex,
  contents: GraphNotificationContents
): string | undefined => {
  const origIndex = contents[0].index.slice(1).split('/');
  const ret = (i: string[]) => `/${i.join('/')}`;
  switch (idx.description) {
    case 'link':
      return '/';
    case 'comment':
      return ret(origIndex.slice(0, 1));
    case 'note':
      return '/';
    case 'mention':
      return undefined;
    default:
      return undefined;
  }
}

export const setMentions = (
  mentions: boolean
): Poke<unknown> => graphHookAction({
  'set-mentions': mentions
});

export const setWatchOnSelf = (
  watchSelf: boolean
): Poke<unknown> => graphHookAction({
  'set-watch-on-self': watchSelf
});

export const setDoNotDisturb = (
  dnd: boolean
): Poke<unknown> => harkAction({
  'set-dnd': dnd
});

export const archive = (
  time: BigInteger,
  index: NotifIndex
): Poke<unknown> => actOnNotification('archive', time, index);

export const read = (
  time: BigInteger,
  index: NotifIndex
): Poke<unknown> => actOnNotification('read-note', time, index);

export const readIndex = (
  index: NotifIndex
): Poke<unknown> => harkAction({
  'read-index': index
});

export const unread = (
  time: BigInteger,
  index: NotifIndex
): Poke<unknown> => actOnNotification('unread-note', time, index);

export const markCountAsRead = (
  association: Association,
  parent: string,
  description: GraphNotifDescription
): Poke<unknown> => harkAction({
  'read-count': {
    graph: {
      graph: association.resource,
      group: association.group,
      description: description,
      index: parent
    }
  }
});

export const markEachAsRead = (
  association: Association,
  parent: string,
  child: string,
  description: GraphNotifDescription,
  module: string
): Poke<unknown> => harkAction({
  'read-each': {
    index: {
      graph: {
        graph: association.resource,
        group: association.group,
        description: description,
        module: module,
        index: parent
      }
    },
    target: child
  }
});

export const dec = (
  index: NotifIndex,
  ref: string
): Poke<unknown> => harkAction({
  dec: {
    index,
    ref
  }
});

export const seen = () => harkAction({ seen: null });

export const readAll = () => harkAction({ 'read-all': null });

export const ignoreGroup = (
  group: string
): Poke<unknown> => groupHookAction({
  ignore: group
});

export const ignoreGraph = (
  graph: string,
  index: string
): Poke<unknown> => graphHookAction({
  ignore: {
    graph,
    index
  }
});

export const listenGroup = (
  group: string
): Poke<unknown> => groupHookAction({
  listen: group
});

export const listenGraph = (
  graph: string,
  index: string
): Poke<unknown> => graphHookAction({
  listen: {
    graph,
    index
  }
});

export const mute = (
  notif: IndexedNotification
): Poke<any> | {} => {
  if('graph' in notif.index && 'graph' in notif.notification.contents) {
    const { index } = notif;
    const parentIndex = getParentIndex(index.graph, notif.notification.contents.graph);
    if(!parentIndex) {
      return {};
    }
    return ignoreGraph(index.graph.graph, parentIndex);
  }
  if('group' in notif.index) {
    const { group } = notif.index.group;
    return ignoreGroup(group);
  }
  return {};
}

export const unmute = (
  notif: IndexedNotification
): Poke<any> | {} => {
  if('graph' in notif.index && 'graph' in notif.notification.contents) {
    const { index } = notif;
    const parentIndex = getParentIndex(index.graph, notif.notification.contents.graph);
    if(!parentIndex) {
      return {};
    }
    return listenGraph(index.graph.graph, parentIndex);
  }
  if('group' in notif.index) {
    return listenGroup(notif.index.group.group);
  }
  return {};
}

export const getLastSeen = (
  unreads: Unreads,
  path: string,
  index: string
): BigInteger | undefined => {
  const lastSeenIdx = unreads.graph?.[path]?.[index]?.unreads;
  if (!(typeof lastSeenIdx === 'string')) {
    return bigInt.zero;
  }
  return f.flow(f.split('/'), f.last, x => (x ? bigInt(x) : undefined))(
    lastSeenIdx
  );
}

export const getUnreadCount = (
  unreads: Unreads,
  path: string,
  index: string
): number => {
  const graphUnreads = unreads.graph?.[path]?.[index]?.unreads ?? 0;
  return typeof graphUnreads === 'number' ? graphUnreads : graphUnreads.size;
}

export const getNotificationCount = (
  unreads: Unreads,
  path: string
): number => {
  const unread = unreads.graph?.[path] || {};
  return Object.keys(unread)
    .map(index => unread[index]?.notifications as number || 0)
    .reduce(f.add, 0);
}
