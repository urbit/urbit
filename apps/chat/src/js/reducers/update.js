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

  reduceMessages(messages, state) {
    console.log(messages);
    if (messages && messages.circle in state.messages) {
      let station = state.messages[messages.circle];
      if (
        station.length > 0 &&
        station[station.length - 1].num === station.length - 1  &&
        messages.start === station.length
      ) {
        state.messages[messages.circle] = 
          state.messages[messages.circle].concat(messages.envelopes);
      } else {
        console.error('%messages has indices inconsistent with localStorage');
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

}

