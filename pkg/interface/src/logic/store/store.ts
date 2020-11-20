import BaseStore from './base';
import InviteReducer from '../reducers/invite-update';
import MetadataReducer from '../reducers/metadata-update';
import LocalReducer from '../reducers/local';
import ChatReducer from '../reducers/chat-update';

import { StoreState } from './type';
import { Timebox } from '~/types';
import { Cage } from '~/types/cage';
import ContactReducer from '../reducers/contact-update';
import S3Reducer from '../reducers/s3-update';
import { GraphReducer } from '../reducers/graph-update';
import { HarkReducer } from '../reducers/hark-update';
import GroupReducer from '../reducers/group-update';
import LaunchReducer from '../reducers/launch-update';
import ConnectionReducer from '../reducers/connection';
import {OrderedMap} from '../lib/OrderedMap';
import { BigIntOrderedMap } from '../lib/BigIntOrderedMap';

export const homeAssociation = {
  "app-path": "/home",
  "app-name": "contact",
  "group-path": "/home",
  metadata: {
    color: "0x0",
    title: "DMs + Drafts",
    description: "",
    "date-created": "",
    module: "",
  },
};


export default class GlobalStore extends BaseStore<StoreState> {
  inviteReducer = new InviteReducer();
  metadataReducer = new MetadataReducer();
  localReducer = new LocalReducer();
  chatReducer = new ChatReducer();
  contactReducer = new ContactReducer();
  s3Reducer = new S3Reducer();
  groupReducer = new GroupReducer();
  launchReducer = new LaunchReducer();
  connReducer = new ConnectionReducer();

  rehydrate() {
    this.localReducer.rehydrate(this.state);
  }

  dehydrate() {
    this.localReducer.dehydrate(this.state);
  }

  initialState(): StoreState {
    return {
      pendingMessages: new Map(),
      chatInitialized: false,
      connection: 'connected',
      sidebarShown: true,
      omniboxShown: false,
      suspendedFocus: null,
      baseHash: null,
      background: undefined,
      remoteContentPolicy: {
        imageShown: true,
        audioShown: true,
        videoShown: true,
        oembedShown: true,
      },
      hideAvatars: false,
      hideNicknames: false,
      invites: {},
      associations: {
        chat: {},
        contacts: {},
        graph: {},
      },
      groups: {},
      groupKeys: new Set(),
      graphs: {},
      graphKeys: new Set(),
      launch: {
        firstTime: false,
        tileOrdering: [],
        tiles: {},
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
      notebooks: {},
      contacts: {},
      dark: false,
      inbox: {},
      chatSynced: null,
      notifications: new BigIntOrderedMap<Timebox>(),
      archivedNotifications: new BigIntOrderedMap<Timebox>(),
      notificationsGroupConfig: [],
      notificationsChatConfig: [],
      notificationsGraphConfig: {
        watchOnSelf: false,
        mentions: false,
        watching: [],
      },
      notificationsCount: 0,
      unreads: {
        graph: {},
        group: {},
        chat: {},
      }
    };
  }

  reduce(data: Cage, state: StoreState) {
    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
    this.chatReducer.reduce(data, this.state);
    this.contactReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    this.groupReducer.reduce(data, this.state);
    this.launchReducer.reduce(data, this.state);
    this.connReducer.reduce(data, this.state);
    GraphReducer(data, this.state);
    HarkReducer(data, this.state);
  }
}
