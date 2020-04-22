import { InitialReducer } from '/reducers/initial';
import { ContactUpdateReducer } from '/reducers/contact-update';
import { GroupUpdateReducer } from '/reducers/group-update';
import { InviteUpdateReducer } from '/reducers/invite-update';
import { PermissionUpdateReducer } from '/reducers/permission-update';
import { MetadataReducer } from '/reducers/metadata-update.js';
import { LocalReducer } from '/reducers/local.js';


class Store {
  constructor() {
    this.state = this.initialState();

    this.initialReducer = new InitialReducer();
    this.groupUpdateReducer = new GroupUpdateReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.contactUpdateReducer = new ContactUpdateReducer();
    this.inviteUpdateReducer = new InviteUpdateReducer();
    this.metadataReducer = new MetadataReducer();
    this.localReducer = new LocalReducer();
    this.setState = () => {};
  }

  initialState() {
    return {
      contacts: {},
      groups: {},
      associations: {},
      permissions: {},
      invites: {},
      selectedGroups: []
    };
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    if ('clear' in json && json.clear) {
      this.setState(this.initialState());
      return;
    }

    console.log(json);
    this.initialReducer.reduce(json, this.state);
    this.groupUpdateReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.contactUpdateReducer.reduce(json, this.state);
    this.inviteUpdateReducer.reduce(json, this.state);
    this.metadataReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
