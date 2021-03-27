import _ from 'lodash';

export class InitialReducer {
  reduce(json, state) {
    console.log('json', json);
    let data = _.get(json, 'initial', false);
    console.log('data', data);
    if (data) {
    }
  }
}
