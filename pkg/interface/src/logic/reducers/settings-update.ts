import { SettingsUpdate } from '@urbit/api/settings';
import _ from 'lodash';
import { SettingsState as State } from '~/logic/state/settings';
import { BaseState } from '../state/base';

type SettingsState = State & BaseState<State>;

function putBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
  const data = _.get(json, 'put-bucket', false);
  if (data) {
    state[data['bucket-key']] = data.bucket;
  }
  return state;
}

function delBucket(json: SettingsUpdate, state: SettingsState): SettingsState {
  const data = _.get(json, 'del-bucket', false);
  if (data) {
    delete state[data['bucket-key']];
  }
  return state;
}

function putEntry(json: SettingsUpdate, state: any): SettingsState {
  const data: Record<string, string> = _.get(json, 'put-entry', false);
  if (data) {
    if (!state[data['bucket-key']]) {
      state[data['bucket-key']] = {};
    }
    state[data['bucket-key']][data['entry-key']] = data.value;
  }
  return state;
}

function delEntry(json: SettingsUpdate, state: any): SettingsState {
  const data = _.get(json, 'del-entry', false);
  if (data) {
    delete state[data['bucket-key']][data['entry-key']];
  }
  return state;
}

function getAll(json: any, state: SettingsState): SettingsState {
  const data = _.get(json, 'all');
  if(data) {
    _.mergeWith(state, data, (obj, src) => _.isArray(src) ? src : undefined);
  }
  return state;
}

function getBucket(json: any, state: SettingsState): SettingsState {
  const key    = _.get(json, 'bucket-key', false);
  const bucket = _.get(json, 'bucket', false);
  if (key && bucket) {
    state[key] = bucket;
  }
  return state;
}

function getEntry(json: any, state: any) {
  const bucketKey = _.get(json, 'bucket-key', false);
  const entryKey  = _.get(json, 'entry-key', false);
  const entry     = _.get(json, 'entry', false);
  if (bucketKey && entryKey && entry) {
    state[bucketKey][entryKey] = entry;
  }
  return state;
}

export const reduceUpdate = [
  putBucket,
  delBucket,
  putEntry,
  delEntry
];

export const reduceScry = [
  getAll,
  getBucket,
  getEntry
];
