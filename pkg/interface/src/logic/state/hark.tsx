import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';

import { NotificationGraphConfig, Timebox, Unreads, dateToDa } from "@urbit/api";
import BigIntOrderedMap from "@urbit/api/lib/BigIntOrderedMap";

// import useApi from "~/logic/lib/useApi";
// import { harkGraphHookReducer, harkGroupHookReducer, harkReducer } from "~/logic/subscription/hark";
import { stateSetter } from "~/logic/lib/util";

export const HARK_FETCH_MORE_COUNT = 3;

export interface HarkState extends State {
  archivedNotifications: BigIntOrderedMap<Timebox>;
  doNotDisturb: boolean;
  // getMore: () => Promise<boolean>;
  // getSubset: (offset: number, count: number, isArchive: boolean) => Promise<void>;
  // getTimeSubset: (start?: Date, end?: Date) => Promise<void>;
  notifications: BigIntOrderedMap<Timebox>;
  notificationsCount: number;
  notificationsGraphConfig: NotificationGraphConfig; // TODO unthread this everywhere
  notificationsGroupConfig: []; // TODO type this
  set: (fn: (state: HarkState) => void) => void;
  unreads: Unreads;
};

const useHarkState = create<HarkState>(persist((set, get) => ({
  archivedNotifications: new BigIntOrderedMap<Timebox>(),
  doNotDisturb: false,
  // getMore: async (): Promise<boolean> => {
  //   const state = get();
  //   const offset = state.notifications.size || 0;
  //   await state.getSubset(offset, HARK_FETCH_MORE_COUNT, false);
  //   // TODO make sure that state has mutated at this point.
  //   return offset === (state.notifications.size || 0);
  // },
  // getSubset: async (offset, count, isArchive): Promise<void> => {
  //   const api = useApi();
  //   const where = isArchive ? 'archive' : 'inbox';
  //   const result = await api.scry({
  //     app: 'hark-store',
  //     path: `/recent/${where}/${offset}/${count}`
  //   });
  //   harkReducer(result);
  //   return;
  // },
  // getTimeSubset: async (start, end): Promise<void> => {
  //   const api = useApi();
  //   const s = start ? dateToDa(start) : '-';
  //   const e = end ? dateToDa(end) : '-';
  //   const result = await api.scry({
  //     app: 'hark-hook',
  //     path: `/recent/${s}/${e}`
  //   });
  //   harkGroupHookReducer(result);
  //   harkGraphHookReducer(result);
  //   return;
  // },
  notifications: new BigIntOrderedMap<Timebox>(),
  notificationsCount: 0,
  notificationsGraphConfig: {
    watchOnSelf: false,
    mentions: false,
    watching: []
  },
  notificationsGroupConfig: [],
  set: fn => stateSetter(fn, set),
  unreads: {
    graph: {},
    group: {}
  },
}), {
  blacklist: ['notifications', 'archivedNotifications', 'unreads'],
  name: 'LandscapeHarkState'
}));

function withHarkState<P, S extends keyof HarkState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const harkState = stateMemberKeys ? useHarkState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useHarkState();
    return <Component ref={ref} {...harkState} {...props} />
  });
}

export { useHarkState as default, withHarkState };