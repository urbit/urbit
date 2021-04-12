import _ from 'lodash';
import useSettingsState, { SettingsState } from "~/logic/state/settings";
import { SettingsUpdate } from '@urbit/api/dist/settings';
import { reduceState } from '../state/base';

export default class SettingsReducer {
  reduce(json: any) {
    let data = json["settings-event"];
    if (data) {
      reduceState<SettingsState, SettingsUpdate>(useSettingsState, data, [
        this.putBucket,
        this.delBucket,
        this.putEntry,
        this.delEntry,
      ]);
    }
    data = json["settings-data"];
    if (data) {
      reduceState<SettingsState, SettingsUpdate>(useSettingsState, data, [
        this.getAll,
        this.getBucket,
        this.getEntry,
      ]);
    }
  }

  putBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
    const data = _.get(json, 'put-bucket', false);
    if (data) {
      state[data["bucket-key"]] = data.bucket;
    }
    return state;
  }

  delBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
    const data = _.get(json, 'del-bucket', false);
    if (data) {
      delete state[data['bucket-key']];
    }
    return state;
  }

  putEntry(json: SettingsUpdate, state: SettingsState): SettingsState {
    const data = _.get(json, 'put-entry', false);
    if (data) {
      if (!state[data["bucket-key"]]) {
        state[data["bucket-key"]] = {};
      }
      state[data["bucket-key"]][data["entry-key"]] = data.value;
    }
    return state;
  }

  delEntry(json: SettingsUpdate, state: SettingsState): SettingsState {
    const data = _.get(json, 'del-entry', false);
    if (data) {
      delete state[data["bucket-key"]][data["entry-key"]];
    }
    return state;
  }

  getAll(json: any, state: SettingsState): SettingsState {
    const data = _.get(json, 'all');
    if(data) {
      _.mergeWith(state, data, (obj, src) => _.isArray(src) ? src : undefined)
    }
    return state;
  }

  getBucket(json: any, state: SettingsState): SettingsState {
    const key    = _.get(json, 'bucket-key', false);
    const bucket = _.get(json, 'bucket', false);
    if (key && bucket) {
      state[key] = bucket;
    }
    return state;
  }

  getEntry(json: any, state: SettingsState) {
    const bucketKey = _.get(json, 'bucket-key', false);
    const entryKey  = _.get(json, 'entry-key', false);
    const entry     = _.get(json, 'entry', false);
    if (bucketKey && entryKey && entry) {
      state[bucketKey][entryKey] = entry;
    }
    return state;
  }
}
