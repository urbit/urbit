import _ from 'lodash';

export class LocalReducer {
  reduce(json, state) {
    let data = _.get(json, 'local', false);
    if (data) {
      this.setSelected(data, state);
    }
  }

  setSelected(json, state) {
    let data = _.has(json, 'selected', false);
    if (data) {
      state.selectedGroups = json.selected;
    }
  }
}