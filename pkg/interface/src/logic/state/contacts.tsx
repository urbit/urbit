import React from "react";
import create, { State }  from 'zustand';
import { persist } from 'zustand/middleware';

import { Patp, Rolodex, Scry } from "@urbit/api";

import { stateSetter } from "~/logic/lib/util";
import useApi from "~/logic/lib/useApi";

export interface ContactState extends State {
  contacts: Rolodex;
  isContactPublic: boolean;
  nackedContacts: Set<Patp>;
  fetchIsAllowed: (entity, name, ship, personal) => Promise<boolean>;
  set: (fn: (state: ContactState) => void) => void;
};

const useContactState = create<ContactState>(persist((set, get) => ({
  contacts: {},
  nackedContacts: new Set(),
  isContactPublic: false,
  fetchIsAllowed: async (
    entity,
    name,
    ship,
    personal
  ): Promise<boolean> => {
    const isPersonal = personal ? 'true' : 'false';
    const api = useApi();
    return api.scry({
      app: 'contact-store',
      path: `/is-allowed/${entity}/${name}/${ship}/${isPersonal}`
    });
  },
  set: fn => stateSetter(fn, set)
}), {
  name: 'ContactReducer'
}));

function withContactState<P, S extends keyof ContactState>(Component: any, stateMemberKeys?: S[]) {
  return React.forwardRef((props: Omit<P, S>, ref) => {
    const contactState = stateMemberKeys ? useContactState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ): useContactState();
    return <Component ref={ref} {...contactState} {...props} />
  });
}

export { useContactState as default, withContactState };