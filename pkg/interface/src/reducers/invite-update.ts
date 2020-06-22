import _ from 'lodash';
import { StoreState } from '../store/type';
import { Cage } from '../types/cage';
import { InviteUpdate } from '../types/invite-update';

type InviteState = Pick<StoreState, "invites">;


export default class InviteReducer<S extends InviteState> {
  reduce(json: Cage, state: S) {
    const data = json['invite-update'];
    if (data) {
      this.initial(data, state);
      this.create(data, state);
      this.delete(data, state);
      this.invite(data, state);
      this.accepted(data, state);
      this.decline(data, state);
    }
  }

  initial(json: InviteUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.invites = data;
    }
  }

  create(json: InviteUpdate, state: S) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.invites[data.path] = {};
    }
  }

  delete(json: InviteUpdate, state: S) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.invites[data.path];
    }
  }

  invite(json: InviteUpdate, state: S) {
    const data = _.get(json, 'invite', false);
    if (data) {
      state.invites[data.path][data.uid] = data.invite;
    }
  }

  accepted(json: InviteUpdate, state: S) {
    const data = _.get(json, 'accepted', false);
    if (data) {
      console.log(data);
      delete state.invites[data.path][data.uid];
    }
  }

  decline(json: InviteUpdate, state: S) {
    const data = _.get(json, 'decline', false);
    if (data) {
      delete state.invites[data.path][data.uid];
    }
  }
}
