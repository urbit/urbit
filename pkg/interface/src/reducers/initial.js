import _ from 'lodash';

export default class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'chat-initial', false);
    if (data) {
      state.inbox = data;
      state.chatInitialized = true;
    }

    data = _.get(json, 'permission-initial', false);
    if (data) {
      for (const perm in data) {
        state.permissions[perm] = {
          who: new Set(data[perm].who),
          kind: data[perm].kind
        };
      }
    }

    data = _.get(json, 'invite-initial', false);
    if (data) {
      state.invites = data;
    }

    data = _.get(json, 'contact-initial', false);
    if (data) {
      state.contacts = data;
    }

    data = _.get(json, 'chat-hook-update', false);
    if (data) {
      state.chatSynced = data;
    }
  }
}
