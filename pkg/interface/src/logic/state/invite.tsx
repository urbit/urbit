import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';

import { Invites } from '@urbit/api';

import { stateSetter } from "../lib/util";

export interface InviteState extends State {
  invites: Invites;
  set: (fn: (state: InviteState) => void) => void;
};

const useInviteState = create<InviteState>(persist((set, get) => ({
  invites: {},
  set: fn => stateSetter(fn, set),
}), {
  name: 'LandscapeInviteState'
}));

function withInviteState<P, S extends keyof InviteState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const inviteState = stateMemberKeys ? useInviteState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useInviteState();
    return <Component ref={ref} {...inviteState} {...props} />
  });
}

export { useInviteState as default, withInviteState };