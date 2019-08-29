import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'inbox-initial', false);
    if (data) {
      state.inbox = data;
    }

    data = _.get(json, 'groups-initial', false);
    if (data) {
      for (let group in data) {
        state.groups[group] = new Set(data[group]);
      }
    }
  }
}

