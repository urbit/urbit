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
import { Contact, getDeskSettings, putEntry, Value } from '@urbit/api';

export interface ShortcutMapping {
  cycleForward: string;
  cycleBack: string;
  navForward: string;
  navBack: string;
  hideSidebar: string;
  readGroup: string;
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
    disableSpellcheck: boolean;
  };
  keyboard: ShortcutMapping;
  remoteContentPolicy: RemoteContentPolicy;
  getAll: () => Promise<void>;
  putEntry: (bucket: string, key: string, value: Value) => Promise<void>;
  leap: {
    categories: LeapCategories[];
  };
  bookmarks: { [key: string]: string }
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
      hideUtilities: false,
      disableSpellcheck: false
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
    keyboard: {
      cycleForward: 'ctrl+\'',
      cycleBack: 'ctrl+;',
      navForward: 'ctrl+]',
      navBack: 'ctrl+[',
      hideSidebar: 'ctrl+\\',
      readGroup: 'shift+Escape'
    },
    bookmarks: {},
    getAll: async () => {
      const { desk } = await airlock.scry(getDeskSettings((window as any).desk));
      get().set((s) => {
        for(const bucket in desk) {
          s[bucket] = { ...(s[bucket] || {}), ...desk[bucket] };
        }
      });
    },
    putEntry: async (bucket: string, entry: string, value: Value) => {
      const poke = putEntry((window as any).desk, bucket, entry, value);
      pokeOptimisticallyN(useSettingsState, poke, reduceUpdate);
    }
  }),
  [],
  [
    (set, get) =>
      createSubscription('settings-store', `/desk/${(window as any).desk}`, (e) => {
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

// Hide is an optional second parameter for when this function is used in class components
export function useShowNickname(contact: Contact | null, hide?: boolean): boolean {
  const hideState = useSettingsState(state => state.calm.hideNicknames);
  const hideNicknames = typeof hide !== 'undefined' ? hide : hideState;
  return Boolean(contact && contact.nickname && !hideNicknames);
}

export default useSettingsState;
