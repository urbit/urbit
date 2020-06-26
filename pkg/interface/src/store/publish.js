import BaseStore from './base';

import ContactReducer from '../reducers/contact-update';
import GroupReducer from '../reducers/group-update';
import LocalReducer from '../reducers/local';
import PublishReducer from '../reducers/publish-update';
import InviteReducer from '../reducers/invite-update';
import PublishResponseReducer from '../reducers/publish-response';
import PermissionReducer from '../reducers/permission-update';
import MetadataReducer from '../reducers/metadata-update';

export default class PublishStore extends BaseStore {
  constructor() {
    super();

    this.contactReducer = new ContactReducer();
    this.groupReducer = new GroupReducer();
    this.localReducer = new LocalReducer();
    this.publishReducer = new PublishReducer();
    this.inviteReducer = new InviteReducer();
    this.responseReducer = new PublishResponseReducer();
    this.permissionReducer = new PermissionReducer();
    this.metadataReducer = new MetadataReducer();
  }

  initialState() {
    return {
      notebooks: {},
      groups: {},
      contacts: {},
      associations: {
        contacts: {}
      },
      permissions: {},
      invites: {},
      sidebarShown: true
    };
  }

  reduce(data, state) {
    this.contactReducer.reduce(data, this.state);
    this.groupReducer.reduce(data, this.state);
    this.localReducer.reduce(data, this.state);
    this.publishReducer.reduce(data, this.state);
    this.permissionReducer.reduce(data, this.state);
    this.metadataReducer.reduce(data, this.state);
    this.inviteReducer.reduce(data, this.state);
    this.responseReducer.reduce(data, this.state);
  }
}

