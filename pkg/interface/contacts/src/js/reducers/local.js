import _ from 'lodash';

export class LocalReducer {
  reduce(json, state) {
    let data = _.get(json, 'local', false);
    if (data) {
      this.setSpinner(data, state);
    }
  }

  setSpinner(json, state) {
    let data = _.has(json, 'spinner', false);
    if (data) {
      state.spinner = json.spinner;
    }
  }
}