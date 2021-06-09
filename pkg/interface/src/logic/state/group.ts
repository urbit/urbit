import { Association, Group, JoinRequests } from '@urbit/api';
import { useCallback } from 'react';
import { reduce } from '../reducers/group-update';
import _ from 'lodash';
import { reduce as reduceView } from '../reducers/group-view';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';

export interface GroupState {
  groups: {
    [group: string]: Group;
  };
  pendingJoin: JoinRequests;
}

// @ts-ignore investigate zustand types
const useGroupState = createState<GroupState>(
  'Group',
  {
    groups: {},
    pendingJoin: {}
  },
  ['groups'],
  [
    (set, get) =>
      createSubscription('group-store', '/groups', (e) => {
        if ('groupUpdate' in e) {
          reduceStateN(get(), e.groupUpdate, reduce);
        }
      }),
      (set, get) => createSubscription('group-view', '/groups', (e) => {
        const data = _.get(e, 'group-view-update', false);
        if (data) {
          reduceStateN(get(), data, reduceView);
        }
      })
  ]
);

export function useGroup(group: string) {
  return useGroupState(
    useCallback(s => s.groups[group] as Group | undefined, [group])
  );
}

export function useGroupForAssoc(association: Association) {
  return useGroupState(
    useCallback(s => s.groups[association.group] as Group | undefined, [
      association
    ])
  );
}

export default useGroupState;
