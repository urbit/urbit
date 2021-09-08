import { decline, Invites } from '@urbit/api';
import { reduce } from '../reducers/invite-update';
import _ from 'lodash';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import { useCallback } from 'react';
import airlock from '~/logic/api';

export interface InviteState {
  invites: Invites;
  declineInvite: (app: string, uid: string) => Promise<void>;
}

const useInviteState = createState<InviteState>(
  'Invite',
  (set, get) => ({
    invites: {},
    declineInvite: async (app, uid) => {
      get().set((s) => {
        delete s.invites[app][uid];
      });
      await airlock.poke(decline(app, uid));
    }
  }),
  ['invites'],
  [
    (set, get) =>
      createSubscription('invite-store', '/all', (e) => {
        const d = _.get(e, 'invite-update', false);
        if (d) {
          reduceStateN(get(), d, reduce);
        }
      })
  ]
);

export function useGroupInvite(group: string) {
  return useInviteState(useCallback((s) => {
    return Object.entries(s.invites.groups || {}).map(([uid, invite]) => {
      return ({ uid, ...invite });
    }).find(({ resource }) => group === `/ship/~${resource.ship}/${resource.name}`);
  }, [group]));
}

export default useInviteState;
