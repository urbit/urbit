import BaseStore from './base';
import InviteReducer from '../reducers/invite-update';
import MetadataReducer from '../reducers/metadata-update';
import LocalReducer from '../reducers/local';
import ChatReducer from '../reducers/chat-update';

import { StoreState } from './type';
import { Cage } from '../types/cage';
import ContactReducer from '../reducers/contact-update';
import LinkUpdateReducer from '../reducers/link-update';
import S3Reducer from '../reducers/s3-update';
import GroupReducer from '../reducers/group-update';
import PermissionReducer from '../reducers/permission-update';
import PublishUpdateReducer from '../reducers/publish-update';
import PublishResponseReducer from '../reducers/publish-response';
import LaunchReducer from '../reducers/launch-update';
import LinkListenReducer from '../reducers/listen-update';


export default class GlobalStore extends BaseStore<StoreState> {
  inviteReducer = new InviteReducer();
  metadataReducer = new MetadataReducer();
  localReducer = new LocalReducer();
  chatReducer = new ChatReducer();
  contactReducer = new ContactReducer();
  linkReducer = new LinkUpdateReducer();
  linkListenReducer = new LinkListenReducer();
  s3Reducer = new S3Reducer();
  groupReducer = new GroupReducer();
  permissionReducer = new PermissionReducer();
  publishUpdateReducer = new PublishUpdateReducer();
  publishResponseReducer = new PublishResponseReducer();
  launchReducer = new LaunchReducer();


  initialState(): StoreState {
    return {
      pendingMessages: new Map(),
      chatInitialized: false,
      sidebarShown: true,
      invites: {},
      associations: {
        chat: {},
        contacts: {},
        link: {},
        publish: {}
      },
      groups: {},
      groupKeys: new Set(),
      launch: {
        firstTime: false,
        tileOrdering: [],
        tiles: {},
      },
      weather: {},
      userLocation: null,
      permissions: {},
      s3: {
        configuration: {
          buckets: new Set(),
          currentBucket: ''
        },
        credentials: null
      },
      links: {},
      linksSeen: {},
      linkListening: new Set(),
      linkComments: {},
      notebooks: {},
      contacts: {},
      selectedGroups: [],
      inbox: {},
      chatSynced: null,
    };
  }

  reduce(data: Cage, state: StoreState) {
    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
    this.chatReducer.reduce(data, this.state);
    this.contactReducer.reduce(data, this.state);
    this.linkReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    this.groupReducer.reduce(data, this.state);
    this.permissionReducer.reduce(data, this.state);
    this.publishUpdateReducer.reduce(data, this.state);
    this.publishResponseReducer.reduce(data, this.state);
    this.launchReducer.reduce(data, this.state);
    this.linkListenReducer.reduce(data, this.state);
  }
}
