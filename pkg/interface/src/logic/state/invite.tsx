import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';
import { Invites } from '@urbit/api/invite/types';
import { accept, decline } from '@urbit/api/invite'; 
import { Serial } from "@urbit/api";
import useApi from "../lib/useApi";

export interface InviteState extends State {
  invites: Invites;
  accept: (app: string, uid: Serial) => Promise<number | void>;
  decline: (app: string, uid: Serial) => Promise<number | void>;
  set: (fn: (state: InviteState) => void) => void;
};

const useInviteState = create<InviteState>(persist((set, get) => ({
  invites: {},
  accept: async (app: string, uid: Serial) => {
    const api = await useApi();
    return api.poke(accept(app, uid));
  },
  decline: async (app: string, uid: Serial) => {
    const api = await useApi();
    return api.poke(decline(app, uid));
  },
  set: fn => set(produce(fn))
}), {
  name: 'InviteReducer'
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