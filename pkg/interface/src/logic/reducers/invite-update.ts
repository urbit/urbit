import _ from 'lodash';
import { compose } from 'lodash/fp';

import { InviteUpdate } from '@urbit/api/invite';

import { Cage } from '~/types/cage';
import useInviteState, { InviteState } from '../state/invite';
import { reduceState } from '../state/base';

export default class InviteReducer {
  reduce(json: Cage) {
    const data = json['invite-update'];
    if (data) {
      reduceState<InviteState, InviteUpdate>(useInviteState, data, [
        initial,
        create,
        deleteInvite,
        invite,
        accepted,
        decline,
      ]);
    }
  }
}

const initial = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'initial', false);
  if (data) {
    state.invites = data;
  }
  return state;
}

const create = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'create', false);
  if (data) {
    state.invites[data] = {};
  }
  return state;
}

const deleteInvite = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'delete', false);
  if (data) {
    delete state.invites[data];
  }
  return state;
}

const invite = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'invite', false);
  if (data) {
    state.invites[data.term][data.uid] = data.invite;
  }
  return state;
}

const accepted = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'accepted', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
}

const decline = (json: InviteUpdate, state: InviteState): InviteState => {
  const data = _.get(json, 'decline', false);
  if (data) {
    delete state.invites[data.term][data.uid];
  }
  return state;
}