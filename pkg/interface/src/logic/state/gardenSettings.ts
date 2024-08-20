import f from 'lodash/fp';
import _ from 'lodash';
import {
  BaseState,
  createState,
  createSubscription,
  reduceStateN,
  optReduceState
} from '~/logic/state/base';
import airlock from '~/logic/api';
import {
  getDeskSettings,
  SettingsUpdate,
  Value,
  Poke,
  putEntry as doPutEntry
} from '@urbit/api';
import { UseStore } from 'zustand';

export interface ShortcutMapping {
  cycleForward: string;
  cycleBack: string;
  navForward: string;
  navBack: string;
  hideSidebar: string;
  readGroup: string;
}

export interface GardenSettingsState {
  browserSettings: {
    settings: string;
  };
  loaded: boolean;
  getAll: () => Promise<void>;
  putEntry: (bucket: string, key: string, value: Value) => Promise<void>;
}

function putBucket(
  json: SettingsUpdate,
  state: GardenSettingsState
): GardenSettingsState {
  const data = _.get(json, 'put-bucket', false);
  if (data) {
    state[data['bucket-key']] = data.bucket;
  }
  return state;
}

function delBucket(
  json: SettingsUpdate,
  state: GardenSettingsState
): GardenSettingsState {
  const data = _.get(json, 'del-bucket', false);
  if (data) {
    delete state[data['bucket-key']];
  }
  return state;
}

function putEntry(json: SettingsUpdate, state: any): GardenSettingsState {
  const data: Record<string, string> = _.get(json, 'put-entry', false);
  if (data) {
    if (!state[data['bucket-key']]) {
      state[data['bucket-key']] = {};
    }
    state[data['bucket-key']][data['entry-key']] = data.value;
  }
  return state;
}

function delEntry(json: SettingsUpdate, state: any): GardenSettingsState {
  const data = _.get(json, 'del-entry', false);
  if (data) {
    delete state[data['bucket-key']][data['entry-key']];
  }
  return state;
}

export async function pokeOptimisticallyN<A, S extends {}>(
  state: UseStore<S & BaseState<S>>,
  poke: Poke<any>,
  reduce: ((a: A, fn: S & BaseState<S>) => S & BaseState<S>)[]
) {
  let num: string | undefined = undefined;
  try {
    num = optReduceState(state, poke.json, reduce);
    await airlock.poke(poke);
    state.getState().removePatch(num);
  } catch (e) {
    console.error(e);
    if (num) {
      state.getState().rollback(num);
    }
  }
}

const reduceUpdate = [putBucket, delBucket, putEntry, delEntry];

export const selectSettingsState = <
  K extends keyof (GardenSettingsState & BaseState<GardenSettingsState>)
>(
  keys: K[]
) => f.pick<BaseState<GardenSettingsState> & GardenSettingsState, K>(keys);

// @ts-ignore investigate zustand types
const useGardenSettingsState = createState<SettingsState>(
  'Settings',
  (set, get) => ({
    browserSettings: {
      settings: ''
    },
    loaded: false,
    getAll: async () => {
      const result = (await airlock.scry(getDeskSettings('garden'))).desk;

      const newState = {
        ..._.mergeWith(get(), result, (obj, src) =>
          _.isArray(src) ? src : undefined
        ),
        loaded: true
      };
      set(newState);
    },
    // getAll: async () => {
    // const { desk } = await airlock.scry(getDeskSettings('garden'));
    // get().set((s) => {
    // for (const bucket in desk) {
    // s[bucket] = { ...(s[bucket] || {}), ...desk[bucket] };
    // }
    // get().set(s)
    // });
    // },
    putEntry: async (bucket: string, entry: string, value: Value) => {
      const poke = doPutEntry('garden', bucket, entry, value);
      pokeOptimisticallyN(useGardenSettingsState, poke, reduceUpdate);
    }
  }),
  [],
  [
    (set, get) =>
      createSubscription('settings-store', '/desk/garden', (e) => {
        const data = _.get(e, 'settings-event', false);
        if (data) {
          reduceStateN(get(), data, reduceUpdate);
          set({ loaded: true });
        }
      })
  ]
);

const selBrowserSettings = (s: GardenSettingsState) =>
  s.browserSettings.settings;
export function useBrowserSettings() {
  const settings = useGardenSettingsState(selBrowserSettings);
  console.log({ settings });
  return settings !== '' ? JSON.parse(settings) : [];
}

export function useProtocolHandling(browserId: string) {
  const settings = useBrowserSettings();
  const { protocolHandling = false } =
    settings.filter((el: any) => el.browserId === browserId)[0] ?? false;
  return protocolHandling;
}

export function useBrowserNotifications(browserId: string) {
  const settings = useBrowserSettings();
  const { browserNotifications = false } =
    settings.filter((el: any) => el.browserId === browserId)[0] ?? false;
  return browserNotifications;
}

export default useGardenSettingsState;
