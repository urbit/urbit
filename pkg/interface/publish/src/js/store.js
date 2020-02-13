import { InitialReducer }    from '/reducers/initial';
import { PrimaryReducer }    from '/reducers/primary';
import { ResponseReducer }   from '/reducers/response';
import { GroupReducer }      from '/reducers/group';
import { InviteReducer }     from '/reducers/invite';
import { PermissionReducer } from '/reducers/permission';

class Store {
  constructor() {
    this.state = {
      notebooks: {},
      groups: {},
      contacts: {},
      permissions: {},
      invites: {},
      spinner: false,
      sidebarShown: true
    }

    this.initialReducer    = new InitialReducer();
    this.primaryReducer    = new PrimaryReducer();
    this.responseReducer   = new ResponseReducer();
    this.groupReducer      = new GroupReducer();
    this.inviteReducer     = new InviteReducer();
    this.permissionReducer = new PermissionReducer();
    this.setState = () => {};

    this.initialReducer.reduce(window.injectedState, this.state);
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(evt) {
    if (evt.from && evt.from.path === '/all') {
      this.groupReducer.reduce(evt.data, this.state);
      this.permissionReducer.reduce(evt.data, this.state);
    }
    else if (evt.from && evt.from.path === '/primary'){
      this.primaryReducer.reduce(evt.data, this.state);
      this.inviteReducer.reduce(evt.data, this.state);
    } else if (evt.type) {
      this.responseReducer.reduce(evt, this.state);
    }
    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
