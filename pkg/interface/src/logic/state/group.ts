import { Path, JoinRequests, Group } from "@urbit/api";

import { BaseState, createState } from "./base";
import {useCallback} from "react";

export interface GroupState extends BaseState<GroupState> {
  groups: {
    [groupPath: string]: Group;
  };
  pendingJoin: JoinRequests;
};

const useGroupState = createState<GroupState>('Group', {
  groups: {},
  pendingJoin: {},
}, ['groups']);

export function useGroup(group: string) {
  return useGroupState(useCallback(s => s.groups[group], [group]));
}

export default useGroupState;
