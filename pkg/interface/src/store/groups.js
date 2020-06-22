import ContactReducer from '../reducers/contact-update';
import GroupReducer from '../reducers/group-update';
import InviteReducer from '../reducers/invite-update';
import PermissionReducer from '../reducers/permission-update';
import MetadataReducer from '../reducers/metadata-update';
import LocalReducer from '../reducers/local';
import S3Reducer from '../reducers/s3-update';

import BaseStore from './base';


export default class GroupsStore extends BaseStore {
  constructor() {
    super();
    this.groupReducer = new GroupReducer();
    this.permissionReducer = new PermissionReducer();
    this.contactReducer = new ContactReducer();
    this.inviteReducer = new InviteReducer();
    this.metadataReducer = new MetadataReducer();
    this.s3Reducer = new S3Reducer();
    this.localReducer = new LocalReducer();
  }

  initialState() {
    return {
      contacts: {},
      groups: {},
      associations: {},
      permissions: {},
      invites: {},
      s3: {}
    };
  }

  reduce(data, state) {
    this.groupReducer.reduce(data, this.state);
    this.permissionReducer.reduce(data, this.state);
    this.contactReducer.reduce(data, this.state);
    this.inviteReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.s3Reducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
  }
}

