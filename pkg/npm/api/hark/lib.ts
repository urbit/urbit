import { BigInteger } from 'big-integer';

import { Poke } from '../lib/types';
import {
  HarkBin,
  HarkBinId,
  HarkLid,
  HarkPlace
} from './types';
import { decToUd } from '../lib';

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
  bin: HarkBin
): Poke<unknown> =>
  harkAction({
    [frond]: {
      time: decToUd(intTime.toString()),
      bin
    }
  });

export const setMentions = (mentions: boolean): Poke<unknown> =>
  graphHookAction({
    'set-mentions': mentions
  });

export const setWatchOnSelf = (watchSelf: boolean): Poke<unknown> =>
  graphHookAction({
    'set-watch-on-self': watchSelf
  });

export const setDoNotDisturb = (dnd: boolean): Poke<unknown> =>
  harkAction({
    'set-dnd': dnd
  });

export const archive = (bin: HarkBin, lid: HarkLid): Poke<unknown> =>
  harkAction({
    archive: {
      lid,
      bin
    }
  });

export const opened = harkAction({
  opened: null
});



export const markCountAsRead = (place: HarkPlace): Poke<unknown> =>
  harkAction({
    'read-count': place
  });

export const markEachAsRead = (
  place: HarkPlace,
  path: string
): Poke<unknown> =>
  harkAction({
    'read-each': {
      place,
      path
    }
  });

export const seen = () => harkAction({ seen: null });

export const readAll = harkAction({ 'read-all': null });
export const archiveAll = harkAction({ 'archive-all': null });

export const ignoreGroup = (group: string): Poke<unknown> =>
  groupHookAction({
    ignore: group
  });

export const ignoreGraph = (graph: string, index: string): Poke<unknown> =>
  graphHookAction({
    ignore: {
      graph,
      index
    }
  });

export const listenGroup = (group: string): Poke<unknown> =>
  groupHookAction({
    listen: group
  });

export const listenGraph = (graph: string, index: string): Poke<unknown> =>
  graphHookAction({
    listen: {
      graph,
      index
    }
  });

/**
 * Read all graphs belonging to a particular group
 */
export const readGroup = (group: string) =>
  harkAction({
    'read-group': group
  });

/**
 * Read all unreads in a graph
 */
export const readGraph = (graph: string) =>
  harkAction({
    'read-graph': graph
  });

export function harkBinToId(bin: HarkBin): HarkBinId {
  const { place, path } = bin;
  return `${place.desk}${place.path}${path}`;
}

export function harkBinEq(a: HarkBin, b: HarkBin): boolean {
  return (
    a.place.path === b.place.path &&
    a.place.desk === b.place.desk &&
    a.path === b.path
  );
}
