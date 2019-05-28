import _ from 'lodash';


export class UpdateReducer {
  reduce(json, state) {
    let data = _.get(json, 'update', false);
    if (data) {
      this.reduceInbox(_.get(data, 'inbox', false), state);
      this.reduceMessage(_.get(data, 'message', false), state);
      this.reduceConfig(_.get(data, 'config', false), state);
      this.reduceCircles(_.get(data, 'circles', false), state);
    }
  }

  reduceInbox(inbox, state) {
    if (inbox) {
      state.inbox = inbox;
    }
  }

  reduceMessage(message, state) {
    if (message.circle in state.messages) {
      state.messages[message.circle].push(message.envelope);
    } else {
      state.messages[message.circle] = [message.envelope];
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

}

