import _ from 'lodash';


export class InitialReducer {
  reduce(json, state) {
    let data = _.get(json, 'initial', false);
    console.log(data);
    if (data) {
      state = data;
    }
  }
}

