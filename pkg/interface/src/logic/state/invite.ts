import { Invites } from '@urbit/api';
import { BaseState, createState } from './base';

export interface InviteState extends BaseState<InviteState> {
  invites: Invites;
};

const useInviteState = createState<InviteState>('Invite', {
  invites: {},
});

export default useInviteState;