import produce from 'immer';
import f from 'lodash/fp';
import React from 'react';
import create, { State } from 'zustand';
import { persist } from 'zustand/middleware';
import { BackgroundConfig, LeapCategories, RemoteContentPolicy, TutorialProgress, tutorialProgress } from '~/types/local-update';
import airlock from '~/logic/api';
import { bootstrapApi } from '../api/bootstrap';
import { wait } from '~/logic/lib/util';

export type SubscriptionStatus = 'connected' | 'disconnected' | 'reconnecting';

export interface LocalState {
  theme: 'light' | 'dark' | 'auto';
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: RemoteContentPolicy;
  tutorialProgress: TutorialProgress;
  hideGroups: boolean;
  hideUtilities: boolean;
  tutorialRef: HTMLElement | null,
  hideTutorial: () => void;
  nextTutStep: () => void;
  prevTutStep: () => void;
  hideLeapCats: LeapCategories[];
  setTutorialRef: (el: HTMLElement | null) => void;
  dark: boolean;
  mobile: boolean;
  breaks: {
    sm: boolean;
    md: boolean;
    lg: boolean;
  }
  background: BackgroundConfig;
  omniboxShown: boolean;
  suspendedFocus?: HTMLElement;
  toggleOmnibox: () => void;
  set: (fn: (state: LocalState) => void) => void;
  subscription: SubscriptionStatus;
  reconnect: () => Promise<void>;
  bootstrap: () => Promise<void>;
  errorCount: number;
}

type LocalStateZus = LocalState & State;

export const selectLocalState =
  <K extends keyof LocalState>(keys: K[]) => f.pick<LocalState, K>(keys);

const useLocalState = create<LocalStateZus>(persist((set, get) => ({
  dark: false,
  mobile: false,
  breaks: {
    sm: false,
    md: false,
    lg: false
  },
  background: undefined,
  theme: 'auto',
  hideAvatars: false,
  hideNicknames: false,
  hideLeapCats: [],
  hideGroups: false,
  hideUtilities: false,
  tutorialProgress: 'hidden',
  tutorialRef: null,
  setTutorialRef: (el: HTMLElement | null) => set(produce((state) => {
    state.tutorialRef = el;
  })),
  hideTutorial: () => set(produce((state) => {
    state.tutorialProgress = 'hidden';
    state.tutorialRef = null;
  })),
  nextTutStep: () => set(produce((state) => {
    const currIdx = tutorialProgress.findIndex(p => p === state.tutorialProgress);
    if(currIdx < tutorialProgress.length) {
      state.tutorialProgress = tutorialProgress[currIdx + 1];
    }
  })),
  prevTutStep: () => set(produce((state) => {
    const currIdx = tutorialProgress.findIndex(p => p === state.tutorialProgress);
    if(currIdx > 0) {
      state.tutorialProgress = tutorialProgress[currIdx - 1];
    }
  })),
  remoteContentPolicy: {
    imageShown: true,
    audioShown: true,
    videoShown: true,
    oembedShown: true
  },
  omniboxShown: false,
  suspendedFocus: undefined,
  toggleOmnibox: () => set(produce((state) => {
    state.omniboxShown = !state.omniboxShown;
    if (typeof state.suspendedFocus?.focus === 'function') {
      state.suspendedFocus.focus();
      state.suspendedFocus = undefined;
    } else {
      state.suspendedFocus = document.activeElement;
      state.suspendedFocus.blur();
    }
  })),
  subscription: 'connected',
  errorCount: 0,
  // XX this logic should be handled by eventsource lib, but channel$a
  // resume doesn't work properly
  reconnect: async () => {
    const { errorCount } = get();
    set(s => ({ errorCount: s.errorCount+1, subscription: 'reconnecting' }));

    if(errorCount > 5) {
      set({ subscription: 'disconnected' });
      return;
    }
    await wait(Math.pow(2, errorCount) * 750);

    try {
      airlock.reset();
      await bootstrapApi();
    } catch (e) {
      console.error(`Retrying connection, attempt #${errorCount}`);
    }
  },
  bootstrap: async () => {
    set({ subscription: 'reconnecting', errorCount: 0 });
    airlock.reset();
    await bootstrapApi();
    set({ subscription: 'connected' });
  },
  // @ts-ignore investigate zustand types
  set: fn => set(produce(fn))
  }), {
    blacklist: [
      'suspendedFocus', 'toggleOmnibox', 'omniboxShown', 'tutorialProgress',
      'prevTutStep', 'nextTutStep', 'tutorialRef', 'setTutorialRef', 'subscription',
      'errorCount', 'breaks'
    ],
  name: 'localReducer'
}));

function withLocalState<P, S extends keyof LocalState, C extends React.ComponentType<P>>(Component: C, stateMemberKeys?: S[]) {
  return React.forwardRef<C, Omit<P, S>>((props, ref) => {
    const localState = stateMemberKeys ? useLocalState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useLocalState();
    // @ts-ignore call signature forwarding unclear
    return <Component ref={ref} {...localState} {...props} />;
  });
}

const selOsDark = (s: LocalState) => s.dark;
export function useOsDark() {
  return useLocalState(selOsDark);
}

export { useLocalState as default, withLocalState };
