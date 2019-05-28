import _ from 'lodash';


export class ChatReducer {
  reduce(json, state) {
    let data = _.get(json, 'chat', false);
    if (data) {
      state.messages = data.messages;
      state.inbox = data.inbox;
      state.configs = data.configs;
      state.circles = data.circles;
    }
  }
}

