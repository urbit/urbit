import { Path, JoinRequests, Association } from "@urbit/api";

import { BaseState, createState } from "./base";
import {useCallback} from "react";

export interface GroupState extends BaseState<GroupState> {
  groups: Set<Path>;
  pendingJoin: JoinRequests;
};

const useGroupState = createState<GroupState>('Group', {
  groups: new Set(),
  pendingJoin: {},
}, ['groups']);

export function useGroupForAssoc(assoc?: Association) {
  return useGroupState(useCallback(s => {
    const group = assoc?.group;
    if(!group) {
      return undefined;
    }
    return s.groups[group]
  }, [assoc]));
}



export default useGroupState;
