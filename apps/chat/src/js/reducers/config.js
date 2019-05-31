import _ from 'lodash';


export class ConfigReducer {
  reduce(json, state) {
    let data = _.get(json, 'chat', false);
    if (data) {
      state.inbox = data.inbox;
      state.configs = data.configs;
      state.circles = data.circles;
    }
  }
}

