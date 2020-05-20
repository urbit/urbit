import _ from 'lodash';

export class InviteReducer {
  reduce(json, state) {
    let initial = _.get(json, 'invite-initial', false);
    if (initial) {
      this.initial(initial, state);
    }
    let update = _.get(json, 'invite-update', false);
    if (update) {
      this.create(update, state);
      this.delete(update, state);
      this.invite(update, state);
      this.accepted(update, state);
      this.decline(update, state);
    }
  }

  initial(json, state) {
    state.invites = json;;
  }

  create(json, state) {
    let data = _.get(json, 'create', false);
    if (data) {
      state.invites[data.path] = {};
    }
  }

  delete(json, state) {
    let data = _.get(json, 'delete', false);
    if (data) {
      delete state.invites[data.path];
    }
  }

  invite(json, state) {
    let data = _.get(json, 'invite', false);
    if (data) {
      state.invites[data.path][data.uid] = data.invite;
    }
  }

  accepted(json, state) {
    let data = _.get(json, 'accepted', false);
    if (data) {
      delete state.invites[data.path][data.uid];
    }
  }

  decline(json, state) {
    let data = _.get(json, 'decline', false);
    if (data) {
      delete state.invites[data.path][data.uid];
    }
  }
}

