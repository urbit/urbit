import React, { ReactNode } from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';
import { BackgroundConfig, RemoteContentPolicy } from "~/types/local-update";

export interface LocalState extends State {
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: RemoteContentPolicy;
  dark: boolean;
  background: BackgroundConfig;
  omniboxShown: boolean;
  suspendedFocus?: HTMLElement;
  toggleOmnibox: () => void;
  set: (fn: (state: LocalState) => void) => void
};

const useLocalState = create<LocalState>(persist((set, get) => ({
  dark: false,
  background: undefined,
  hideAvatars: false,
  hideNicknames: false,
  remoteContentPolicy: {
    imageShown: true,
    audioShown: true,
    videoShown: true,
    oembedShown: true,
  },
  omniboxShown: false,
  suspendedFocus: undefined,
  toggleOmnibox: () => set(produce(state => {
    state.omniboxShown = !state.omniboxShown;
    if (typeof state.suspendedFocus?.focus === 'function') {
      state.suspendedFocus.focus();
      state.suspendedFocus = undefined;
    } else {
      state.suspendedFocus = document.activeElement;
      state.suspendedFocus.blur();
    }
  })),
  set: fn => set(produce(fn))
}), {
  name: 'localReducer'
}));

function withLocalState<P, S extends keyof LocalState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const localState = stateMemberKeys ? useLocalState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useLocalState();
    return <Component ref={ref} {...localState} {...props} />
  });
}

export { useLocalState as default, withLocalState };