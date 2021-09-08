import { accept, Association, Group, JoinRequests } from '@urbit/api';
import { useCallback } from 'react';
import { reduce } from '../reducers/group-update';
import _ from 'lodash';
import { reduce as reduceView } from '../reducers/group-view';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import airlock from '~/logic/api';
import { join } from '../../../../npm/api/groups';

export interface GroupState {
  groups: {
    [group: string]: Group;
  };
  pendingJoin: JoinRequests;
  joinGroup: (grp: string, uid?: string) => Promise<void>;
}

// @ts-ignore investigate zustand types
const useGroupState = createState<GroupState>(
  'Group',
  ((set, get) => ({
    groups: {},
    pendingJoin: {},
    joinGroup: async (group, uid) => {
      set((s) => {
        s.pendingJoin[group] = {
          hidden: false,
          started: Date.now(),
          ship: '~zod',
          progress: 'start'
        };
      });
      const [,,ship,name] = group.split('/');
      await airlock.poke(join(ship,name));
      if(uid) {
        await airlock.poke(accept('groups', uid));
      }
    }

  })),
  ['groups'],
  [
    (set, get) =>
      createSubscription('group-store', '/groups', (e) => {
        if ('groupUpdate' in e) {
          reduceStateN(get(), e.groupUpdate, reduce);
        }
      }),
      (set, get) => createSubscription('group-view', '/all', (e) => {
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

export function useJoiningGroup(group: string) {
  return useGroupState(
    useCallback(s => s.pendingJoin[group], [group])
  );
}

export default useGroupState;
