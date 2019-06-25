import _ from 'lodash';


export class UpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'update', false);
    if (data) {
      this.reduceInbox(_.get(data, 'inbox', false), state);
      this.reduceMessage(_.get(data, 'message', false), state);
      this.reduceMessages(_.get(data, 'messages', false), state);
      this.reduceConfig(_.get(data, 'config', false), state);
      this.reduceCircles(_.get(data, 'circles', false), state);
      this.reducePeers(_.get(data, 'peers', false), state);
      this.reduceDelete(_.get(data, 'delete', false), state);
    }
  }

  reduceInbox(inbox, state) {
    if (inbox) {
      state.inbox = inbox;
    }
  }

  reduceMessage(message, state) {
    if (message && message.circle in state.messages) {
      state.messages[message.circle].push(message.envelope);
    } else {
      state.messages[message.circle] = [message.envelope];
    }
  }

  reduceMessages(msgs, state) {
    if (msgs) {

      let staMsgs = state.messages[msgs.circle];
      if (msgs.circle in state.messages) {
        console.log('new messages object: ', msgs);
        console.log('lowest num in store: ', staMsgs[0].num);
        console.log('highest num in store: ', staMsgs[staMsgs.length - 1].num);

        if (staMsgs.length > 0 && staMsgs[0].num - 1 === msgs.end) {
          state.messages[msgs.circle] = msgs.envelopes.concat(staMsgs);

        } else if (staMsgs.length === 0) {
          state.messages[msgs.circle] = msgs.envelopes;

        } else {
          console.error('%messages has inconsistent indices');

        }

      } else {
        state.messages[msgs.circle] = msgs.envelopes;
      }
    }

  }

  reduceConfig(config, state) {
    if (config) {
      state.configs[config.circle] = config.config;
    }
  }

  reduceCircles(circles, state) {
    if (circles) {
      state.circles = circles;
    }
  }

  reducePeers(peers, state) {
    if (peers) {
      state.peers[peers.circle] = peers.peers;
    }
  }

  reduceDelete(del, state) {
    if (del) {
      delete state.configs[del.circle];
      state.messages[del.circle] = [];
    }
  }

}

