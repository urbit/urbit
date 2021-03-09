import _ from 'lodash';

import BaseStore from './base';
import InviteReducer from '../reducers/invite-update';
import MetadataReducer from '../reducers/metadata-update';
import LocalReducer from '../reducers/local';

import { StoreState } from './type';
import { Timebox } from '@urbit/api';
import { Cage } from '~/types/cage';
import S3Reducer from '../reducers/s3-update';
import { GraphReducer } from '../reducers/graph-update';
import { HarkReducer } from '../reducers/hark-update';
import { ContactReducer } from '../reducers/contact-update';
import GroupReducer from '../reducers/group-update';
import LaunchReducer from '../reducers/launch-update';
import ConnectionReducer from '../reducers/connection';
import SettingsReducer from '../reducers/settings-update';
import GcpReducer from '../reducers/gcp-reducer';
import { OrderedMap } from '../lib/OrderedMap';
import { BigIntOrderedMap } from '../lib/BigIntOrderedMap';
import { GroupViewReducer } from '../reducers/group-view';

export default class GlobalStore extends BaseStore<StoreState> {
  inviteReducer = new InviteReducer();
  metadataReducer = new MetadataReducer();
  localReducer = new LocalReducer();
  s3Reducer = new S3Reducer();
  groupReducer = new GroupReducer();
  launchReducer = new LaunchReducer();
  connReducer = new ConnectionReducer();
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
      invites: {},
      associations: {
        groups: {},
        graph: {}
      },
      groups: {},
      groupKeys: new Set(),
      graphs: {},
      graphKeys: new Set(),
      launch: {
        firstTime: false,
        tileOrdering: [],
        tiles: {}
      },
      weather: {},
      userLocation: null,
      storage: {
        gcp: {},
        s3: {
          configuration: {
            buckets: new Set(),
            currentBucket: ''
          },
          credentials: null
        },
      },
      isContactPublic: false,
      contacts: {},
      nackedContacts: new Set(),
      notifications: new BigIntOrderedMap<Timebox>(),
      archivedNotifications: new BigIntOrderedMap<Timebox>(),
      notificationsGroupConfig: [],
      notificationsGraphConfig: {
        watchOnSelf: false,
        mentions: false,
        watching: []
      },
      unreads: {
        graph: {},
        group: {}
      },
      notificationsCount: 0,
      settings: {},
      pendingJoin: {},
      graphTimesentMap: {}
    };
  }

  reduce(data: Cage, state: StoreState) {
    //  debug shim
    const tag = Object.keys(data)[0];
    const oldActions = this.pastActions[tag] || [];
    this.pastActions[tag] = [data[tag], ...oldActions.slice(0,14)];

    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    this.groupReducer.reduce(data, this.state);
    this.launchReducer.reduce(data, this.state);
    this.connReducer.reduce(data, this.state);
    GraphReducer(data, this.state);
    HarkReducer(data, this.state);
    ContactReducer(data, this.state);
    this.settingsReducer.reduce(data);
    this.gcpReducer.reduce(data, this.state);
    GroupViewReducer(data, this.state);
  }
}
