import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'contact-initial', false);
    if (data) {
      state.contacts = data;
    }

    data = _.get(json, 'group-initial', false);
    if (data) {
      for (let group in data) {
        state.groups[group] = new Set(data[group]);
      }
    }
  }
}

