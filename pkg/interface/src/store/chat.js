import ContactReducer from '../reducers/contact-update';
import ChatReducer from '../reducers/chat-update';
import InviteReducer from '../reducers/invite-update';
import PermissionReducer from '../reducers/permission-update';
import MetadataReducer from '../reducers/metadata-update';
import S3Reducer from '../reducers/s3-update';
import LocalReducer from '../reducers/local';

import BaseStore from './base';

export default class ChatStore extends BaseStore {
  constructor() {
    super();
    this.permissionReducer = new PermissionReducer();
    this.contactReducer = new ContactReducer();
    this.chatReducer = new ChatReducer();
    this.inviteReducer = new InviteReducer();
    this.s3Reducer = new S3Reducer();
    this.metadataReducer = new MetadataReducer();
    this.localReducer = new LocalReducer();
  }

  initialState() {
    return {
      inbox: {},
      chatSynced: null,
      contacts: {},
      permissions: {},
      invites: {},
      associations: {
        chat: {},
        contacts: {}
      },
      sidebarShown: true,
      pendingMessages: new Map([]),
      chatInitialized: false,
      s3: {}
    };
  }

  reduce(data, state) {
    this.permissionReducer.reduce(data, this.state);
    this.contactReducer.reduce(data, this.state);
    this.chatReducer.reduce(data, this.state);
    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    console.log(this.state);
  }
}

