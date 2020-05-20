import _ from 'lodash';

export default class LaunchReducer {
  reduce(json, state) {
    const data = _.get(json, 'launch-update', false);
    if (data) {
      this.log(data, state);
    }
  }

  log(json, state) {
    console.log(json);
  }
}
