import _ from 'lodash';

import BaseStore from './base';
import LocalReducer from '../reducers/local';

import { StoreState } from './type';
import { Cage } from '~/types/cage';
import S3Reducer from '../reducers/s3-update';
import LaunchReducer from '../reducers/launch-update';
import ConnectionReducer from '../reducers/connection';
import SettingsReducer from '../reducers/settings-update';

export default class GlobalStore extends BaseStore<StoreState> {
  localReducer = new LocalReducer();
  s3Reducer = new S3Reducer();
  launchReducer = new LaunchReducer();
  connReducer = new ConnectionReducer();
  settingsReducer = new SettingsReducer();

  pastActions: Record<string, any> = {}

  constructor() {
    super();
    (window as any).debugStore = this.debugStore.bind(this);
  }

  debugStore(tag: string, ...stateKeys: string[]) {
    console.log(this.pastActions[tag]);
    console.log(_.pick(this.state, stateKeys));
  }

  rehydrate() {
    this.localReducer.rehydrate(this.state);
  }

  dehydrate() {
    this.localReducer.dehydrate(this.state);
  }

  initialState(): StoreState {
    return {
      connection: 'connected',
      baseHash: null,
      launch: {
        firstTime: false,
        tileOrdering: [],
        tiles: {}
      },
      weather: {},
      userLocation: null,
      s3: {
        configuration: {
          buckets: new Set(),
          currentBucket: ''
        },
        credentials: null
      },
      
      settings: {}
    };
  }

  reduce(data: Cage, state: StoreState) {
    //  debug shim
    const tag = Object.keys(data)[0];
    const oldActions = this.pastActions[tag] || [];
    this.pastActions[tag] = [data[tag], ...oldActions.slice(0,14)];

    this.localReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    this.launchReducer.reduce(data, this.state);
    this.connReducer.reduce(data, this.state);
    this.settingsReducer.reduce(data, this.state);
  }
}
