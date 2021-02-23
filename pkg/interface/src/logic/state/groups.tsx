import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';

import { JoinRequests } from '@urbit/api/groups'; 
import { Path } from "@urbit/api";

import { stateSetter } from "~/logic/lib/util";

export interface GroupState extends State {
  groups: Set<Path>;
  pendingJoin: JoinRequests;
  set: (fn: (state: GroupState) => void) => void;
};

const useGroupState = create<GroupState>(persist((set, get) => ({
  groups: new Set(),
  pendingJoin: {},
  set: fn => stateSetter(fn, set)
}), {
  blacklist: ['groups'],
  name: 'GroupReducer'
}));

function withGroupState<P, S extends keyof GroupState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const groupState = stateMemberKeys ? useGroupState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useGroupState();
    return <Component ref={ref} {...groupState} {...props} />
  });
}

export { useGroupState as default, withGroupState };