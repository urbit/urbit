import f from 'lodash/fp';
import _ from 'lodash';
import {
  RemoteContentPolicy,
  LeapCategories,
  leapCategories
} from '~/types/local-update';
import { useShortcut as usePlainShortcut } from '~/logic/lib/shortcutContext';
import {
  BaseState,
  createState,
  createSubscription,
  reduceStateN,
  pokeOptimisticallyN
} from '~/logic/state/base';
import { useCallback } from 'react';
import { reduceUpdate } from '../reducers/settings-update';
import airlock from '~/logic/api';
import { getAll, Value } from '@urbit/api';
import { putEntry } from '@urbit/api/settings';

export interface ShortcutMapping {
  cycleForward: string;
  cycleBack: string;
  navForward: string;
  navBack: string;
  hideSidebar: string;
  readGroup: string;
  leap: string;
}

export interface SettingsState {
  display: {
    backgroundType: 'none' | 'url' | 'color';
    background?: string;
    dark: boolean;
    theme: 'light' | 'dark' | 'auto';
  };
  calm: {
    hideNicknames: boolean;
    hideAvatars: boolean;
    hideUnreads: boolean;
    hideGroups: boolean;
    hideUtilities: boolean;
  };
  keyboard: ShortcutMapping;
  remoteContentPolicy: RemoteContentPolicy;
  getAll: () => Promise<void>;
  putEntry: (bucket: string, key: string, value: Value) => void;
  leap: {
    categories: LeapCategories[];
  };
  tutorial: {
    seen: boolean;
    joined?: number;
  };
}

export const selectSettingsState = <K extends keyof (SettingsState & BaseState<SettingsState>)>(keys: K[]) =>
  f.pick<BaseState<SettingsState> & SettingsState, K>(keys);

export const selectCalmState = (s: SettingsState) => s.calm;

export const selectDisplayState = (s: SettingsState) => s.display;

// @ts-ignore investigate zustand types
const useSettingsState = createState<SettingsState>(
  'Settings',
  (set, get) => ({
    display: {
      backgroundType: 'none',
      background: undefined,
      dark: false,
      theme: 'auto'
    },
    calm: {
      hideNicknames: false,
      hideAvatars: false,
      hideUnreads: false,
      hideGroups: false,
      hideUtilities: false
    },
    remoteContentPolicy: {
      imageShown: true,
      oembedShown: true,
      audioShown: true,
      videoShown: true
    },
    leap: {
      categories: leapCategories
    },
    tutorial: {
      seen: true,
      joined: undefined
    },
    keyboard: {
      cycleForward: 'ctrl+\'',
      cycleBack: 'ctrl+;',
      navForward: 'ctrl+]',
      navBack: 'ctrl+[',
      hideSidebar: 'ctrl+\\',
      readGroup: 'shift+Escape',
      leap: 'meta+/'
    },
    getAll: async () => {
      const { all } = await airlock.scry(getAll);
      get().set((s) => {
        Object.assign(s, all);
      });
    },
    putEntry: (bucket: string, entry: string, value: Value) => {
      const poke = putEntry(bucket, entry, value);
      pokeOptimisticallyN(useSettingsState, poke, reduceUpdate);
    }
  }),
  [],
  [
    (set, get) =>
      createSubscription('settings-store', '/all', (e) => {
        const data = _.get(e, 'settings-event', false);
        if (data) {
          reduceStateN(get(), data, reduceUpdate);
        }
      })
  ]
);

export function useShortcut<T extends keyof ShortcutMapping>(
  name: T,
  cb: (e: KeyboardEvent) => void
) {
  const key = useSettingsState(useCallback(s => s.keyboard[name], [name]));
  return usePlainShortcut(key, cb);
}

const selTheme = (s: SettingsState) => s.display.theme;

export function useTheme() {
  return useSettingsState(selTheme);
}

export default useSettingsState;
