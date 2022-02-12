import {
  archive,
  BigIntOrderedMap,
  HarkBin,
  markCountAsRead,
  NotificationGraphConfig,
  Unreads,
  Timebox,
  HarkLid,
  harkBinToId,
  decToUd,
  unixToDa,
  opened,
  markEachAsRead
} from '@urbit/api';
import { Poke } from '@urbit/http-api';
import { patp2dec } from 'urbit-ob';
import _ from 'lodash';
import api from '~/logic/api';
import { useCallback, useMemo } from 'react';

import {
  createState,
  createSubscription,
  pokeOptimisticallyN,
  reduceStateN
} from './base';
import { reduce, reduceGraph, reduceGroup } from '../reducers/hark-update';
import useMetadataState from './metadata';

export const HARK_FETCH_MORE_COUNT = 3;

export interface HarkState {
  archive: BigIntOrderedMap<Timebox>;
  doNotDisturb: boolean;
  poke: (poke: Poke<any>) => Promise<void>;
  getMore: () => Promise<boolean>;
  opened: () => void;
  // getTimeSubset: (start?: Date, end?: Date) => Promise<void>;
  unseen: Timebox;
  seen: Timebox;
  notificationsCount: number;
  notificationsGraphConfig: NotificationGraphConfig; // TODO unthread this everywhere
  notificationsGroupConfig: string[];
  unreads: Unreads;
  archiveNote: (bin: HarkBin, lid: HarkLid) => Promise<void>;
  readCount: (path: string) => Promise<void>;
  readGraph: (graph: string) => Promise<void>;
  readGroup: (group: string) => Promise<void>;
}

const useHarkState = createState<HarkState>(
  'Hark',
  (set, get) => ({
    archive: new BigIntOrderedMap<Timebox>(),
    doNotDisturb: false,
    unreadNotes: [],
    poke: async (poke: Poke<any>) => {
      await pokeOptimisticallyN(useHarkState, poke, [reduce]);
    },
    readGraph: async (graph: string) => {
      const prefix = `/graph/${graph.slice(6)}`;
      let counts = [] as string[];
      let eaches = [] as [string, string][];
      Object.entries(get().unreads).forEach(([path, unreads]) => {
        if (path.startsWith(prefix)) {
          if(unreads.count > 0) {
            counts.push(path);
          }
          unreads.each.forEach(unread => {
            eaches.push([path, unread]);
          });
        }
      });
      get().set(draft => {
        counts.forEach(path => {
          draft.unreads[path].count = 0;
        });
        eaches.forEach(([path, each]) => {
          draft.unreads[path].each = [];
        });
      });
      await Promise.all([
        ...counts.map(path => markCountAsRead({ desk: window.desk, path })),
        ...eaches.map(([path, each]) => markEachAsRead({ desk: window.desk, path }, each))
      ].map(pok => api.poke(pok)));
    },
    readGroup: async (group: string) => {
      const graphs = 
        _.pickBy(useMetadataState.getState().associations.graph, a => a.group === group);
      await Promise.all(Object.keys(graphs).map(get().readGraph));
    },
    readCount: async (path) => {
      const poke = markCountAsRead({ desk: (window as any).desk, path });
      await pokeOptimisticallyN(useHarkState, poke, [reduce]);
    },
    opened: async () => {
      reduceStateN(get(), { opened: null }, [reduce]);
      await api.poke(opened);
    },
    archiveNote: async (bin: HarkBin, lid: HarkLid) => {
      const poke = archive(bin, lid);
      get().set((draft) => {
        const key = 'seen' in lid ? 'seen' : 'unseen';
        const binId = harkBinToId(bin);
        delete draft[key][binId];
      });
      await api.poke(poke);
    },
    getMore: async (): Promise<boolean> => {
      const state = get();
      const oldSize = state.archive?.size || 0;
      const offset = decToUd(
        state.archive?.peekSmallest()?.[0].toString()
        || unixToDa(Date.now() * 1000).toString()
      );
      const update = await api.scry({
        app: 'hark-store',
        path: `/recent/inbox/${offset}/5`
      });
      reduceStateN(useHarkState.getState(), update, [reduce]);
      return get().archive?.size === oldSize;
    },
    unseen: {},
    seen: {},
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
    'seen',
    'unseen',
    'archive',
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

export const emptyHarkStats = () => ({
      last: 0,
      count: 0,
      each: []
    });

export function useHarkDm(ship: string) {
  return useHarkState(
    useCallback(
      (s) => {
        const key = `/graph/~${window.ship}/dm-inbox/${patp2dec(ship)}`;
        return s.unreads[key] || emptyHarkStats();
      },
      [ship]
    )
  );
}

export function useHarkStat(path: string) {
  return useHarkState(
    useCallback(s => s.unreads[path] || emptyHarkStats(), [path])
  );
}

export function selHarkGraph(graph: string) {
  const [,, ship, name] = graph.split('/');
  const path = `/graph/${ship}/${name}`;
  return (s: HarkState) => (s.unreads[path] || emptyHarkStats());
}

export function useHarkGraph(graph: string) {
  const sel = useMemo(() => selHarkGraph(graph), [graph]);
  return useHarkState(sel);
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

export default useHarkState;
