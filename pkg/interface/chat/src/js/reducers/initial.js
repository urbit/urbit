import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'initial', false);
    if (data) {
      state.messages = data.messages;
      state.inbox = data.inbox;
      state.configs = data.configs;
      state.circles = data.circles;
      state.peers = data.peers;
    }
  }
}

