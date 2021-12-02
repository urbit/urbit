import { deSig, Invite, Invites } from '@urbit/api';
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

export function useInviteForResource(app: string, ship: string, name: string) {
  const { invites } = useInviteState();
  const matches = Object.entries(invites?.[app] || {})
    .reduce((acc, [uid, invite]) => {
      const isMatch = (invite.resource.ship === deSig(ship)
        && invite.resource.name === name)
      return isMatch ? [invite, ...acc] : acc;
    }, [] as Invite[])
  return matches?.[0];
}
