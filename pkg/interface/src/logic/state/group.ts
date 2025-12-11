import { Association, Group, JoinRequests, abortJoin } from '@urbit/api';
import { useCallback } from 'react';
import { reduce } from '../reducers/group-update';
import _ from 'lodash';
import { reduce as reduceView } from '../reducers/group-view';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import api from '~/logic/api';

export interface GroupState {
  groups: {
    [group: string]: Group;
  };
  pendingJoin: JoinRequests;
  abortJoin: (group: string) => Promise<void>;
  doneJoin: (group: string) => Promise<void>;
}

// @ts-ignore investigate zustand types
const useGroupState = createState<GroupState>(
  'Group',
  (set, get) => ({
    groups: {},
    pendingJoin: {},
    abortJoin: async (group) => {
      get().set((draft) => {
        delete draft.pendingJoin[group];
      });
      await api.poke(abortJoin(group));
    },
    doneJoin: async (group) => {
      get().set((draft) => {
        delete draft.pendingJoin[group];
      });
      await api.poke({ app: 'group-view', mark: 'group-view-action', json: {
        done: group
      }});
    },

  }),
  [],
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

export default useGroupState;
