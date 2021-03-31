import { Path, JoinRequests, Group } from "@urbit/api";

import { BaseState, createState } from "./base";

export interface GroupState extends BaseState<GroupState> {
  groups: { [rid: string]: Group; };
  pendingJoin: JoinRequests;
};

const useGroupState = createState<GroupState>('Group', {
  groups: {},
  pendingJoin: {},
}, ['groups']);

export default useGroupState;
