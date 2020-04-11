import { InitialReducer } from '/reducers/initial';
import { ContactUpdateReducer } from '/reducers/contact-update';
import { ChatUpdateReducer } from '/reducers/chat-update';
import { InviteUpdateReducer } from '/reducers/invite-update';
import { PermissionUpdateReducer } from '/reducers/permission-update';
import { MetadataReducer } from '/reducers/metadata-update.js';
import { LocalReducer } from '/reducers/local.js';


class Store {
  constructor() {
    this.state = {
      inbox: {},
      chatSynced: {},
      contacts: {},
      permissions: {},
      invites: {},
      associations: {
        chat: {},
        contacts: {}
      },
      selectedGroups: [],
      sidebarShown: true,
      pendingMessages: new Map([]),
      chatInitialized: false
    };

    this.initialReducer = new InitialReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.contactUpdateReducer = new ContactUpdateReducer();
    this.chatUpdateReducer = new ChatUpdateReducer();
    this.inviteUpdateReducer = new InviteUpdateReducer();
    this.metadataReducer = new MetadataReducer();
    this.localReducer = new LocalReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    console.log(json);
    this.initialReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.contactUpdateReducer.reduce(json, this.state);
    this.chatUpdateReducer.reduce(json, this.state);
    this.inviteUpdateReducer.reduce(json, this.state);
    this.metadataReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
