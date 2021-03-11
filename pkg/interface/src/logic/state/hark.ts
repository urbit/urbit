import { NotificationGraphConfig, Timebox, Unreads, dateToDa } from "@urbit/api";
import BigIntOrderedMap from "@urbit/api/lib/BigIntOrderedMap";

// import { harkGraphHookReducer, harkGroupHookReducer, harkReducer } from "~/logic/subscription/hark";
import { BaseState, createState } from "./base";

export const HARK_FETCH_MORE_COUNT = 3;

export interface HarkState extends BaseState<HarkState> {
  archivedNotifications: BigIntOrderedMap<Timebox>;
  doNotDisturb: boolean;
  // getMore: () => Promise<boolean>;
  // getSubset: (offset: number, count: number, isArchive: boolean) => Promise<void>;
  // getTimeSubset: (start?: Date, end?: Date) => Promise<void>;
  notifications: BigIntOrderedMap<Timebox>;
  notificationsCount: number;
  notificationsGraphConfig: NotificationGraphConfig; // TODO unthread this everywhere
  notificationsGroupConfig: []; // TODO type this
  unreads: Unreads;
};

const useHarkState = createState<HarkState>('Hark', {
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
  unreads: {
    graph: {},
    group: {}
  },
}, ['notifications', 'archivedNotifications', 'unreads', 'notificationsCount']);


export default useHarkState;
