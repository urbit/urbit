import _ from 'lodash';
import useSettingsState, { SettingsState } from "~/logic/state/settings";
import { SettingsUpdate } from '@urbit/api/dist/settings';
import { reduceState } from '../state/base';
import { SubscriptionRequestInterface, UrbitInterface } from '@urbit/http-api';
import { handleSubscriptionError, handleSubscriptionQuit } from '../lib/subscriptionHandlers';

const SettingsReducer = (json: any) => {
  let data = json["settings-event"];
  if (data) {
    useSettingsState.getState().set(state => {
      state = reduceState<SettingsState, SettingsUpdate>(useSettingsState, data, [
        putBucket,
        delBucket,
        putEntry,
        delEntry,
      ]);
    })
  }
  data = json["settings-data"];
  if (data) {
    useSettingsState.getState().set(state => {
      state = reduceState<SettingsState, SettingsUpdate>(useSettingsState, data, [
        getAll,
        getBucket,
        getEntry,
      ]);
    })
  }
}

const putBucket = (json: SettingsUpdate, state: SettingsState): SettingsState => {
  const data = _.get(json, 'put-bucket', false);
  if (data) {
    state[data["bucket-key"]] = data.bucket;
  }
  return state;
}

const delBucket = (json: SettingsUpdate, state: SettingsState): SettingsState => {
  const data = _.get(json, 'del-bucket', false);
  if (data) {
    delete state[data['bucket-key']];
  }
  return state;
}

const putEntry = (json: SettingsUpdate, state: SettingsState): SettingsState => {
  const data = _.get(json, 'put-entry', false);
  if (data) {
    if (!state[data["bucket-key"]]) {
      state[data["bucket-key"]] = {};
    }
    state[data["bucket-key"]][data["entry-key"]] = data.value;
  }
  return state;
}

const delEntry = (json: SettingsUpdate, state: SettingsState): SettingsState => {
  const data = _.get(json, 'del-entry', false);
  if (data) {
    delete state[data["bucket-key"]][data["entry-key"]];
  }
  return state;
}

const getAll = (json: any, state: SettingsState): SettingsState => {
  const data = _.get(json, 'all');
  if(data) {
    _.mergeWith(state, data, (obj, src) => _.isArray(src) ? src : undefined)
  }
  return state;
}

const getBucket = (json: any, state: SettingsState): SettingsState => {
  const key    = _.get(json, 'bucket-key', false);
  const bucket = _.get(json, 'bucket', false);
  if (key && bucket) {
    state[key] = bucket;
  }
  return state;
}

const getEntry = (json: any, state: SettingsState): SettingsState => {
  const bucketKey = _.get(json, 'bucket-key', false);
  const entryKey  = _.get(json, 'entry-key', false);
  const entry     = _.get(json, 'entry', false);
  if (bucketKey && entryKey && entry) {
    state[bucketKey][entryKey] = entry;
  }
  return state;
}

export const settingsSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = SettingsReducer;
  const err = handleSubscriptionError(channel, settingsSubscription);
  const quit = handleSubscriptionQuit(channel, settingsSubscription);
  return {
    app: 'settings-store',
    path: '/all',
    event, err, quit
  };
}