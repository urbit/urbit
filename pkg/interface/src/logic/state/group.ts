import { Association, Group, JoinRequests } from '@urbit/api';
import { useCallback } from 'react';
import { BaseState, createState } from './base';

export interface GroupState extends BaseState<GroupState> {
  groups: {
    [group: string]: Group;
  }
  pendingJoin: JoinRequests;
}

// @ts-ignore investigate zustand types
const useGroupState = createState<GroupState>('Group', {
  groups: {},
  pendingJoin: {}
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
