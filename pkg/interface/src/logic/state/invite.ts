import { Invites } from '@urbit/api';
import { reduce } from '../reducers/invite-update';
import _ from 'lodash';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';

export interface InviteState {
  invites: Invites;
}

const useInviteState = createState<InviteState>(
  'Invite',
  {
    invites: {}
  },
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

export default useInviteState;
