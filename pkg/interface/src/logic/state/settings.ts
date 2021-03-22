import f from 'lodash/fp';
import { RemoteContentPolicy, LeapCategories, leapCategories } from "~/types/local-update";
import { BaseState, createState } from '~/logic/state/base';


export interface SettingsState extends BaseState<SettingsState> {
  display: {
    backgroundType: 'none' | 'url' | 'color';
    background?: string;
    dark: boolean;
    theme: "light" | "dark" | "auto";
  };
  calm: {
    hideNicknames: boolean;
    hideAvatars: boolean;
    hideUnreads: boolean;
    hideGroups: boolean;
    hideUtilities: boolean;
  };
  remoteContentPolicy: RemoteContentPolicy;
  leap: {
    categories: LeapCategories[];
  };
  tutorial: {
    seen: boolean;
    joined?: number;
  };
};

export const selectSettingsState =
<K extends keyof SettingsState>(keys: K[]) => f.pick<SettingsState, K>(keys);

export const selectCalmState = (s: SettingsState) => s.calm;

export const selectDisplayState = (s: SettingsState) => s.display;

const useSettingsState = createState<SettingsState>('Settings', {
  display: {
    backgroundType: 'none',
    background: undefined,
    dark: false,
    theme: "auto"
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
    categories: leapCategories,
  },
  tutorial: {
    seen: false,
    joined: undefined
  }
});

export default useSettingsState;
