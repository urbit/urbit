import { InviteUpdate } from '@urbit/api/invite';
import _ from 'lodash';
import { BaseState } from '../state/base';
import { InviteState as State } from '../state/invite';

type InviteState = State & BaseState<State>;

const initial = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.invites = data;
  }
  return state;
};

const create = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'create', false);
  if (data) {
    state.invites[data] = {};
  }
  return state;
};

const deleteInvite = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'delete', false);
  if (data) {
    delete state.invites[data];
  }
  return state;
};

const invite = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'invite', false);
  if (data) {
    state.invites[data.term][data.uid] = data.invite;
  }
  return state;
};

const accepted = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'accepted', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
};

const decline = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'decline', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
};

export const reduce = [
  initial,
  create,
  deleteInvite,
  invite,
  accepted,
  decline
];
