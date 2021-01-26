import _ from 'lodash';
import { StoreState } from '../../store/type';
import {
  SettingsUpdate,
} from '~/types/settings';

type SettingsState = Pick<StoreState, 'settings'>;

export default class SettingsReducer<S extends SettingsState>{
  reduce(json: Cage, state: S) {
    let data = json["settings-event"];
    if (data) {
      this.putBucket(data, state);
      this.delBucket(data, state);
      this.putEntry(data, state);
      this.delEntry(data, state);
    }
    data = json["settings-data"];
    if (data) {
      this.getAll(data, state);
      this.getBucket(data, state);
      this.getEntry(data, state);
    }
  }

  putBucket(json: SettingsUpdate, state: S) {
    const data = _.get(json, 'put-bucket', false);
    if (data) {
      state.settings[data["bucket-key"]] = data.bucket;
    }
  }

  delBucket(json: SettingsUpdate, state: S) {
    const data = _.get(json, 'del-bucket', false);
    if (data) {
      delete state.settings[data["bucket-key"]];
    }
  }

  putEntry(json: SettingsUpdate, state: S) {
    const data = _.get(json, 'put-entry', false);
    if (data) {
      if (!state.settings[data["bucket-key"]]) {
        state.settings[data["bucket-key"]] = {};
      }
      state.settings[data["bucket-key"]][data["entry-key"]] = data.value;
    }
  }

  delEntry(json: SettingsUpdate, state: S) {
    const data = _.get(json, 'del-entry', false);
    if (data) {
      delete state.settings[data["bucket-key"]][data["entry-key"]];
    }
  }

  getAll(json: any, state: S) {
    state.settings = json;
  }

  getBucket(json: any, state: S) {
    const key    = _.get(json, 'bucket-key', false);
    const bucket = _.get(json, 'bucket', false);
    if (key && bucket) {
      state.settings[key] = bucket;
    }
  }

  getEntry(json: any, state: S) {
    const bucketKey = _.get(json, 'bucket-key', false);
    const entryKey  = _.get(json, 'entry-key', false);
    const entry     = _.get(json, 'entry', false);
    if (bucketKey && entryKey && entry) {
      state.settings[bucketKey][entryKey] = entry;
    }
  }
}
