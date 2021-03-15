import _ from 'lodash';

import BaseStore from './base';

import { StoreState } from './type';
import { Cage } from '~/types/cage';
import S3Reducer from '../reducers/s3-update';
import SettingsReducer from '../reducers/settings-update';
import GcpReducer from '../reducers/gcp-reducer';

export default class GlobalStore extends BaseStore<StoreState> {
  s3Reducer = new S3Reducer();
  settingsReducer = new SettingsReducer();
  gcpReducer = new GcpReducer();

  pastActions: Record<string, any> = {}

  constructor() {
    super();
    (window as any).debugStore = this.debugStore.bind(this);
  }

  debugStore(tag: string, ...stateKeys: string[]) {
    console.log(this.pastActions[tag]);
    console.log(_.pick(this.state, stateKeys));
  }

  initialState(): StoreState {
    return {};
  }

  reduce(data: Cage, state: StoreState) {
    //  debug shim
    const tag = Object.keys(data)[0];
    const oldActions = this.pastActions[tag] || [];
    this.pastActions[tag] = [data[tag], ...oldActions.slice(0,14)];
    this.s3Reducer.reduce(data);
    this.settingsReducer.reduce(data);
    this.gcpReducer.reduce(data);
  }
}
