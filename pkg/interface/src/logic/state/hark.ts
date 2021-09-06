import {
  archive,
  HarkBin,
  markCountAsRead,
  Notification,
  NotificationGraphConfig,
  readNote,
  Unreads
} from '@urbit/api';
import { Poke } from '@urbit/http-api';
import { patp2dec } from 'urbit-ob';
import _ from 'lodash';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import api from '~/logic/api';
import { useCallback, useMemo } from 'react';

import {
  createState,
  createSubscription,
  pokeOptimisticallyN,
  reduceState,
  reduceStateN
} from './base';
import { reduce, reduceGraph, reduceGroup } from '../reducers/hark-update';
import { BigInteger } from 'big-integer';

export const HARK_FETCH_MORE_COUNT = 3;

export interface HarkState {
  archivedNotifications: BigIntOrderedMap<Notification[]>;
  doNotDisturb: boolean;
  poke: (poke: Poke<any>) => Promise<void>;
  getMore: () => Promise<boolean>;
  getSubset: (
    offset: number,
    count: number,
    isArchive: boolean
  ) => Promise<void>;
  // getTimeSubset: (start?: Date, end?: Date) => Promise<void>;
  notifications: BigIntOrderedMap<Notification[]>;
  unreadNotes: Notification[];
  notificationsCount: number;
  notificationsGraphConfig: NotificationGraphConfig; // TODO unthread this everywhere
  notificationsGroupConfig: string[];
  unreads: Unreads;
  archive: (bin: HarkBin, time?: BigInteger) => Promise<void>;
  readNote: (bin: HarkBin) => Promise<void>;
  readCount: (path: string) => Promise<void>;
}

const useHarkState = createState<HarkState>(
  'Hark',
  (set, get) => ({
    archivedNotifications: new BigIntOrderedMap<Notification[]>(),
    doNotDisturb: false,
    unreadNotes: [],
    poke: async (poke: Poke<any>) => {
      await pokeOptimisticallyN(useHarkState, poke, [reduce]);
    },
    readCount: async (path) => {
      const poke = markCountAsRead({ desk: (window as any).desk, path });
      await pokeOptimisticallyN(useHarkState, poke, [reduce]);
    },
    archive: async (bin: HarkBin, time?: BigInteger) => {
      const poke = archive(bin, time);
      await pokeOptimisticallyN(useHarkState, poke, [reduce]);
    },
    readNote: async (bin) => {
      await pokeOptimisticallyN(useHarkState, readNote(bin), [reduce]);
    },
    getMore: async (): Promise<boolean> => {
      const state = get();
      const offset = state.notifications.size || 0;
      await state.getSubset(offset, HARK_FETCH_MORE_COUNT, false);
      const newState = get();
      return offset === (newState?.notifications?.size || 0);
    },
    getSubset: async (offset, count, isArchive): Promise<void> => {
      const where = isArchive ? 'archive' : 'inbox';
      const { harkUpdate } = await api.scry({
        app: 'hark-store',
        path: `/recent/${where}/${offset}/${count}`
      });
      reduceState(useHarkState, harkUpdate, [reduce]);
    },
    notifications: new BigIntOrderedMap<Notification[]>(),
    notificationsCount: 0,
    notificationsGraphConfig: {
      watchOnSelf: false,
      mentions: false,
      watching: []
    },
    notificationsGroupConfig: [],
    unreads: {}
  }),
  [
    'unreadNotes',
    'notifications',
    'archivedNotifications',
    'unreads',
    'notificationsCount'
  ],
  [
    (set, get) =>
      createSubscription('hark-store', '/updates', (d) => {
        reduceStateN(get(), d, [reduce]);
      }),
    (set, get) =>
      createSubscription('hark-graph-hook', '/updates', (j) => {
        const graphHookData = _.get(j, 'hark-graph-hook-update', false);
        if (graphHookData) {
          reduceStateN(get(), graphHookData, reduceGraph);
        }
      }),
    (set, get) =>
      createSubscription('hark-group-hook', '/updates', (j) => {
        const data = _.get(j, 'hark-group-hook-update', false);
        if (data) {
          reduceStateN(get(), data, reduceGroup);
        }
      })
  ]
);

const emptyStats = () => ({
      last: 0,
      count: 0,
      each: []
    });

export function useHarkDm(ship: string) {
  return useHarkState(
    useCallback(
      (s) => {
        return s.unreads[`/graph/~${window.ship}/dm-inbox`]?.[
          `/${patp2dec(ship)}`
        ] || emptyStats();
      },
      [ship]
    )
  );
}

export function useHarkStat(path: string) {
  return useHarkState(
    useCallback(s => s.unreads[path] || emptyStats(), [path])
  );
}

export function useHarkGraph(graph: string) {
  const [, ship, name] = useMemo(() => graph.split('/'), [graph]);
  return useHarkState(
    useCallback(s => s.unreads[`/graph/${ship}/${name}`], [ship, name])
  );
}

export function useHarkGraphIndex(graph: string, index: string) {
  const [, ship, name] = useMemo(() => graph.split('/'), [graph]);
  return useHarkState(
    useCallback(s => s.unreads[`/graph/${ship}/${name}/index`], [
      ship,
      name,
      index
    ])
  );
}

window.hark = useHarkState.getState;
export default useHarkState;
