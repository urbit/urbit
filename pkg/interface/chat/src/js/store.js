import { InitialReducer } from '/reducers/initial';
import { GroupUpdateReducer } from '/reducers/group-update';
import { InboxUpdateReducer } from '/reducers/inbox-update';
import { PermissionUpdateReducer } from '/reducers/permission-update';


class Store {
  constructor() {
    this.state = {
      inbox: {},
      groups: {},
      permissions: {}
    };

    this.initialReducer = new InitialReducer();
    this.groupUpdateReducer = new GroupUpdateReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.inboxUpdateReducer = new InboxUpdateReducer();
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
    this.inboxUpdateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
