import _ from 'lodash';
import { SettingsUpdate } from '~/types/settings';
import useSettingsState, { SettingsStateZus } from "~/logic/state/settings";
import produce from 'immer';

export default class SettingsStateZusettingsReducer{
  reduce(json: any) {
    const old = useSettingsState.getState();
    const newState = produce(old, state => {
      let data = json["settings-event"];
      if (data) {
        console.log(data);
        this.putBucket(data, state);
        this.delBucket(data, state);
        this.putEntry(data, state);
        this.delEntry(data, state);
      }
      data = json["settings-data"];
      if (data) {
        console.log(data);
        this.getAll(data, state);
        this.getBucket(data, state);
        this.getEntry(data, state);
      }
    });
    useSettingsState.setState(newState);
  }

  putBucket(json: SettingsUpdate, state: SettingsStateZus) {
    const data = _.get(json, 'put-bucket', false);
    if (data) {
      state[data["bucket-key"]] = data.bucket;
    }
  }

  delBucket(json: SettingsUpdate, state: SettingsStateZus) {
    const data = _.get(json, 'del-bucket', false);
    if (data) {
      delete settings[data['bucket-key']];
    }
  }

  putEntry(json: SettingsUpdate, state: SettingsStateZus) {
    const data = _.get(json, 'put-entry', false);
    if (data) {
      if (!state[data["bucket-key"]]) {
        state[data["bucket-key"]] = {};
      }
      state[data["bucket-key"]][data["entry-key"]] = data.value;
    }
  }

  delEntry(json: SettingsUpdate, state: SettingsStateZus) {
    const data = _.get(json, 'del-entry', false);
    if (data) {
      delete state[data["bucket-key"]][data["entry-key"]];
    }
  }

  getAll(json: any, state: SettingsStateZus) {
    const data = _.get(json, 'all');
    if(data) {
      _.mergeWith(state, data, (obj, src) => _.isArray(src) ? src : undefined)
    }
  }

  getBucket(json: any, state: SettingsStateZus) {
    const key    = _.get(json, 'bucket-key', false);
    const bucket = _.get(json, 'bucket', false);
    if (key && bucket) {
      state[key] = bucket;
    }
  }

  getEntry(json: any, state: SettingsStateZus) {
    const bucketKey = _.get(json, 'bucket-key', false);
    const entryKey  = _.get(json, 'entry-key', false);
    const entry     = _.get(json, 'entry', false);
    if (bucketKey && entryKey && entry) {
      state[bucketKey][entryKey] = entry;
    }
  }
}
