import { Path, JoinRequests, Association, Group } from "@urbit/api";

import { BaseState, createState } from "./base";
import {useCallback} from "react";

export interface GroupState extends BaseState<GroupState> {
  groups: {
    [group: string]: Group;
  }
  pendingJoin: JoinRequests;
};

const useGroupState = createState<GroupState>('Group', {
  groups: {},
  pendingJoin: {},
}, ['groups']);

export function useGroup(group: string) {
  return useGroupState(useCallback(s => s.groups[group] as Group | undefined, [group]));
}

export function useGroupForAssoc(association: Association) {
  return useGroupState(
    useCallback(s => s.groups[association.group] as Group | undefined, [association])
  );
}

export default useGroupState;
