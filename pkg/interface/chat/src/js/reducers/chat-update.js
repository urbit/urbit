import _ from 'lodash';


export class ChatUpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'chat-update', false);
    if (data) {
      this.pending(data, state);
      this.message(data, state);
      this.messages(data, state);
      this.read(data, state);
      this.create(data, state);
      this.delete(data, state);
    }
  }

  message(json, state) {
    let data = _.get(json, 'message', false);
    if (data) {
      state.inbox[data.path].envelopes.push(data.envelope);
      state.inbox[data.path].config.length
        = state.inbox[data.path].config.length + 1;
    }
  }

  messages(json, state) {
    let data = _.get(json, 'messages', false);
    if (data) {
      state.inbox[data.path].envelopes =
        data.envelopes.concat(state.inbox[data.path].envelopes);
      state.inbox[data.path].config.length =
        state.inbox[data.path].config.length + data.envelopes.length;
    }
  }

  read(json, state) {
    let data = _.get(json, 'read', false);
    if (data) {
      state.inbox[data.path].config.read =
        state.inbox[data.path].config.length;
    }
  }

  create(json, state) {
    let data = _.get(json, 'create', false);
    if (data) {
      state.inbox[`/~${data.ship}${data.path}`] = {
        envelopes: [],
        config: {
          read: 0,
          length: 0
        }
      };
    }
  }

  delete(json, state) {
    let data = _.get(json, 'delete', false);
    if (data) {
      delete state.inbox[data.path];
    }
  }

  pending(json, state) {
    let msg = _.get(json, 'message', false);
    if (!msg || !state.pendingMessages.has(msg.path)) {
      return;
    }

    let mailbox = state.pendingMessages.get(msg.path);

    for (let pendingMsg of mailbox) {
      if (msg.envelope.uid === pendingMsg.uid) {
        let index = mailbox.indexOf(pendingMsg);
        state.pendingMessages.get(msg.path).splice(index, 1);
      }
    }
  }

}

