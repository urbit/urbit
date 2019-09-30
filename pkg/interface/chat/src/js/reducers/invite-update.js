import _ from 'lodash';


export class InviteUpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'invite-update', false);
    if (data) {
      this.create(data, state);
      this.delete(data, state);
      this.invite(data, state);
      this.accept(data, state);
      this.decline(data, state);
    }
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

  accept(json, state) {
    let data = _.get(json, 'accept', false);
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

