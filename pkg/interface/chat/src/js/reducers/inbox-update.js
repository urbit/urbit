import _ from 'lodash';


export class InboxUpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'inbox-update', false);
    if (data) {
      this.message(data, state);
      this.read(data, state);
      this.create(data, state);
      this.delete(data, state);
    }
  }

  message(json, state) {
    let data = _.get(json, 'message', false);
    if (data) {
      state.inbox[data.path].envelopes.push(data.envelope);
    }
  }

  read(json, state) {
    let data = _.get(json, 'read', false);
    if (data) {
      state.inbox[data.path].read = data.read;
    }
  }

  create(json, state) {
    let data = _.get(json, 'create', false);
    if (data) {
      state.inbox[data.path] = {
        envelopes: [],
        read: 0,
        owner: data.owner
      };
    }
  }

  delete(json, state) {
    let data = _.get(json, 'delete', false);
    if (data) {
      delete state.inbox[data.path];
    }
  }
  
}

