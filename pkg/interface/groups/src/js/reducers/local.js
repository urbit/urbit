import _ from 'lodash';

export class LocalReducer {
  reduce(json, state) {
    let data = _.get(json, 'local', false);
    if (data) {
      this.setSpinner(data, state);
      this.setSelected(data, state);
    }
  }

  setSpinner(json, state) {
    let data = _.has(json, 'spinner', false);
    if (data) {
      state.spinner = json.spinner;
    }
  }
  setSelected(json, state) {
    let data = _.has(json, 'selected', false);
    if (data) {
      state.selected = json.selected;
    }
  }
}