import { InitialReducer } from '/reducers/initial';
import { ContactUpdateReducer } from '/reducers/contact-update.js';
import { PermissionUpdateReducer } from '/reducers/permission-update';
import { LinkUpdateReducer } from '/reducers/link-update';
import { LocalReducer } from '/reducers/local.js';
import _ from 'lodash';


class Store {
  constructor() {
    this.state = {
      contacts: {},
      groups: {},
      links: {},
      comments: {},
      seen: {},
      permissions: {},
      sidebarShown: true,
      spinner: false
    };

    this.initialReducer = new InitialReducer();
    this.contactUpdateReducer = new ContactUpdateReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.localReducer = new LocalReducer();
    this.linkUpdateReducer = new LinkUpdateReducer();
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
    this.contactUpdateReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);
    this.linkUpdateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
