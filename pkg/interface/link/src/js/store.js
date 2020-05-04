import { InitialReducer } from '/reducers/initial';
import { GroupUpdateReducer } from '/reducers/group-update';
import { ContactUpdateReducer } from '/reducers/contact-update.js';
import { PermissionUpdateReducer } from '/reducers/permission-update';
import { MetadataReducer } from '/reducers/metadata-update.js';
import { InviteUpdateReducer } from '/reducers/invite-update';
import { LinkUpdateReducer } from '/reducers/link-update';
import { ListenUpdateReducer } from '/reducers/listen-update';
import { LocalReducer } from '/reducers/local.js';
import _ from 'lodash';


class Store {
  constructor() {
    this.state = {
      contacts: {},
      groups: {},
      associations: {
        link: {},
        contacts: {}
      },
      invites: {},
      selectedGroups: [],
      links: {},
      listening: new Set(),
      comments: {},
      seen: {},
      permissions: {},
      sidebarShown: true
    };

    this.initialReducer = new InitialReducer();
    this.groupUpdateReducer = new GroupUpdateReducer();
    this.contactUpdateReducer = new ContactUpdateReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.metadataReducer = new MetadataReducer();
    this.inviteUpdateReducer = new InviteUpdateReducer();
    this.localReducer = new LocalReducer();
    this.linkUpdateReducer = new LinkUpdateReducer();
    this.listenUpdateReducer = new ListenUpdateReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json;
    if (data.data) {
      json = data.data;
    } else {
      json = data;
    }

    console.log('event', json);
    this.initialReducer.reduce(json, this.state);
    this.groupUpdateReducer.reduce(json, this.state);
    this.contactUpdateReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.metadataReducer.reduce(json, this.state);
    this.inviteUpdateReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);
    this.linkUpdateReducer.reduce(json, this.state);
    this.listenUpdateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
