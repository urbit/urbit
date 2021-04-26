import _ from 'lodash';

export class SettingsReducer {
  reduce(json, state) {
    let data = _.get(json, 'bucket', false);
    if (data) {
      state.showWarning = data.warning;
      state.loadedSettings = true;
      if (state.loadedBtc) {
        state.loaded = true;
      }
    }
  }
}
