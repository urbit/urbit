import { Path, JoinRequests } from "@urbit/api";

import { BaseState, createState } from "./base";

export interface GroupState extends BaseState<GroupState> {
  groups: Set<Path>;
  pendingJoin: JoinRequests;
};

const useGroupState = createState<GroupState>('Group', {
  groups: new Set(),
  pendingJoin: {},
}, ['groups']);

export default useGroupState;