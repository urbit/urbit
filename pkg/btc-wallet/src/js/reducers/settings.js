import _ from 'lodash';

export class SettingsReducer {
  reduce(json, state) {
    let data = _.get(json, 'bucket', false);
    if (data) {
      let warning = _.get(json, 'bucket.warning', -1);
      if (warning !== -1) {
        state.showWarning = warning
      }
      let currency = _.get(json, 'bucket.currency', -1);
      if (currency !== -1) {
        state.denomination = currency;
      }

      state.loadedSettings = true;
      if (state.loadedBtc) {
        state.loaded = true;
      }
    }
    let entry = _.get(json, 'settings-event.put-entry.entry-key', false);
    if (entry === 'currency') {
      let value = _.get(json, 'settings-event.put-entry.value', false);
      state.denomination = value;
    } else if (entry === 'warning') {
      let value = _.get(json, 'settings-event.put-entry.value', false);
      state.showWarning = value;
    }
  }
}
