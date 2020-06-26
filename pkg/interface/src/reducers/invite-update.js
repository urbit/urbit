import _ from 'lodash';

export default class InviteReducer {
  reduce(json, state) {
    const data = _.get(json, 'invite-update', false);
    if (data) {
      console.log(data);
      this.initial(data, state);
      this.create(data, state);
      this.delete(data, state);
      this.invite(data, state);
      this.accepted(data, state);
      this.decline(data, state);
    }
  }

  initial(json, state) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.invites = data;
    }
  }

  create(json, state) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.invites[data.path] = {};
    }
  }

  delete(json, state) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.invites[data.path];
    }
  }

  invite(json, state) {
    const data = _.get(json, 'invite', false);
    if (data) {
      state.invites[data.path][data.uid] = data.invite;
    }
  }

  accepted(json, state) {
    const data = _.get(json, 'accepted', false);
    if (data) {
      console.log(data);
      delete state.invites[data.path][data.uid];
    }
  }

  decline(json, state) {
    const data = _.get(json, 'decline', false);
    if (data) {
      delete state.invites[data.path][data.uid];
    }
  }
}
