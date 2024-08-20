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
  loaded: boolean;
}

const useInviteState = createState<InviteState>(
  'Invite',
  {
    invites: {},
    loaded: false
  },
  ['invites', 'loaded'],
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

interface InviteWithUid extends Invite {
  uid: string;
}

export function useInviteForResource(app: string, ship: string, name: string) {
  const { invites } = useInviteState();
  const matches = Object.entries(invites?.[app] || {})
    .reduce((acc, [uid, invite]) => {
      const isMatch = (invite.resource.ship === deSig(ship)
        && invite.resource.name === name)
      return isMatch ? [{ uid, ...invite}, ...acc] : acc;
    }, [] as InviteWithUid[])
  return matches?.[0];
}
