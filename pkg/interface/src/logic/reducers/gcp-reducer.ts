import {StoreState} from '../store/type';
import type {GcpToken} from '../../types/gcp-state';
import type {Cage} from '~/types/cage';
import useStorageState, { StorageState } from '../state/storage';
import { reduceState } from '../state/base';

export default class GcpReducer {
  reduce(json: Cage) {
    reduceState<StorageState, any>(useStorageState, json, [
      reduceConfigured,
      reduceToken
    ]);
  }
}

const reduceConfigured = (json, state: StorageState): StorageState => {
  let data = json['gcp-configured'];
  if (data !== undefined) {
    state.gcp.configured = data;
  }
  return state;
}

const reduceToken = (json: Cage, state: StorageState): StorageState => {
  let data = json['gcp-token'];
  if (data) {
    state = setToken(data, state);
  }
  return state;
}

const setToken = (data: any, state: StorageState): StorageState => {
  if (isToken(data)) {
    state.gcp.token = data;
  }
  return state;
}

const isToken = (token: any): token is GcpToken => {
  return (typeof(token.accessKey) === 'string' &&
          typeof(token.expiresIn) === 'number');
}
