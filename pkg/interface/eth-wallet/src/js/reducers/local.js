import _ from 'lodash';

export class LocalReducer {
  reduce(json, state) {
    let data = _.get(json, 'local', false);
    if (data) {
      this.setFilters(data, state);
    }
  }

  setFilters(obj, state) {
    let eventFilters = _.get(obj, 'eventFilters', false);
    if (eventFilters) {
      state.eventFilters = eventFilters;
    }
  }
}
