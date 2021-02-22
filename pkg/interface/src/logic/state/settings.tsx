import React, { ReactNode } from "react";
import f from 'lodash/fp';
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';
import { BackgroundConfig, RemoteContentPolicy, TutorialProgress, tutorialProgress, LeapCategories, leapCategories } from "~/types/local-update";


export interface SettingsState {
  display: {
    backgroundType: 'none' | 'url' | 'color';
    background?: string;
    dark: boolean;
  };
  calm: {
    hideNicknames: boolean;
    hideAvatars: boolean;
  };
  remoteContentPolicy: RemoteContentPolicy;
  leap: {
    categories: string;
  }
  set: (fn: (state: SettingsState) => void) => void
};

export type SettingsStateZus = SettingsState & State;

export const selectSettingsState = 
<K extends keyof SettingsState>(keys: K[]) => f.pick<SettingsState, K>(keys);

export const selectCalmState = (s: SettingsState) => s.calm;

const useSettingsState = create<SettingsStateZus>((set) => ({
  display: {
    backgroundType: 'none',
    background: undefined,
    dark: false,
  },
  calm: {
    hideNicknames: false,
    hideAvatars: false,
  },
  remoteContentPolicy: {
    imageShown: true,
    oembedShown: true,
    audioShown: true,
    videoShown: true
  },
  leap: {
    categories: JSON.stringify(leapCategories),
  },
  set: (fn: (state: SettingsState) => void) => set(produce(fn))
})); 

function withSettingsState<P, S extends keyof SettingsState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const localState = stateMemberKeys 
      ? useSettingsState(selectSettingsState(stateMemberKeys))
      : useSettingsState();
    return <Component ref={ref} {...localState} {...props} />
  });
}

export { useSettingsState as default, withSettingsState };
