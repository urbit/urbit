/* eslint-disable no-param-reassign */
import {
  SettingsUpdate,
  Value,
  putEntry as doPutEntry,
  getDeskSettings,
  DeskData
} from '@urbit/api';
import _ from 'lodash';
import {
  BaseState,
  createState,
  createSubscription,
  pokeOptimisticallyN,
  reduceStateN
} from './base';
import api from './api';

interface BaseSettingsState {
  display: {
    theme: 'light' | 'dark' | 'auto';
    doNotDisturb: boolean;
  };
  tiles: {
    order: string[];
  };
  loaded: boolean;
  browserSettings: {
    settings: string;
  };
  putEntry: (bucket: string, key: string, value: Value) => Promise<void>;
  fetchAll: () => Promise<void>;
  [ref: string]: unknown;
}

export type SettingsState = BaseSettingsState & BaseState<BaseSettingsState>;

function putBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
  const data = _.get(json, 'put-bucket', false);
  if (data) {
    state[data['bucket-key']] = data.bucket;
  }
  return state;
}

function delBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
  const data = _.get(json, 'del-bucket', false);
  if (data) {
    delete state[data['bucket-key']];
  }
  return state;
}

function putEntry(json: SettingsUpdate, state: any): SettingsState {
  const data: Record<string, string> = _.get(json, 'put-entry', false);
  if (data) {
    if (!state[data['bucket-key']]) {
      state[data['bucket-key']] = {};
    }
    state[data['bucket-key']][data['entry-key']] = data.value;
  }
  return state;
}

function delEntry(json: SettingsUpdate, state: any): SettingsState {
  const data = _.get(json, 'del-entry', false);
  if (data) {
    delete state[data['bucket-key']][data['entry-key']];
  }
  return state;
}

export const reduceUpdate = [putBucket, delBucket, putEntry, delEntry];

export const useSettingsState = createState<BaseSettingsState>(
  'Settings',
  (set, get) => ({
    display: {
      theme: 'auto',
      doNotDisturb: true
    },
    tiles: {
      order: []
    },
    browserSettings: {
      settings: ''
    },
    loaded: false,
    putEntry: async (bucket, key, val) => {
      const poke = doPutEntry(window.desk, bucket, key, val);
      await pokeOptimisticallyN(useSettingsState, poke, reduceUpdate);
    },
    fetchAll: async () => {
      const result = (await api.scry<DeskData>(getDeskSettings(window.desk))).desk;
      const newState = {
        ..._.mergeWith(get(), result, (obj, src) => (_.isArray(src) ? src : undefined)),
        loaded: true
      };
      set(newState);
    }
  }),
  [],
  [
    (set, get) =>
      createSubscription('settings-store', `/desk/${window.desk}`, (e) => {
        const data = _.get(e, 'settings-event', false);
        if (data) {
          reduceStateN(get(), data, reduceUpdate);
          set({ loaded: true });
        }
      })
  ]
);

const selTheme = (s: SettingsState) => s.display.theme;
export function useTheme() {
  return useSettingsState(selTheme);
}

const selBrowserSettings = (s: SettingsState) => s.browserSettings.settings;
export function useBrowserSettings() {
  const settings = useSettingsState(selBrowserSettings);
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
