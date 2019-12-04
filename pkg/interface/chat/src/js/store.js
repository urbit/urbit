import { InitialReducer } from '/reducers/initial';
import { GroupUpdateReducer } from '/reducers/group-update';
import { ChatUpdateReducer } from '/reducers/chat-update';
import { InviteUpdateReducer } from '/reducers/invite-update';
import { PermissionUpdateReducer } from '/reducers/permission-update';
import { LocalReducer } from '/reducers/local.js';


class Store {
  constructor() {
    this.state = {
      inbox: {},
      groups: {},
      permissions: {},
      invites: {},
      spinner: false,
      sidebarShown: true,
      pendingMessages: new Map([])
    };

    this.initialReducer = new InitialReducer();
    this.groupUpdateReducer = new GroupUpdateReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.chatUpdateReducer = new ChatUpdateReducer();
    this.inviteUpdateReducer = new InviteUpdateReducer();
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
    this.groupUpdateReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.chatUpdateReducer.reduce(json, this.state);
    this.inviteUpdateReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
