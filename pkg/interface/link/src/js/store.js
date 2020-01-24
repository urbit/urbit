import { InitialReducer } from '/reducers/initial';
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
      permissions: {},
      sidebarShown: true,
      spinner: false
    };

    this.initialReducer = new InitialReducer();
    this.permissionUpdateReducer = new PermissionUpdateReducer();
    this.localReducer = new LocalReducer();
    this.linkUpdateReducer = new LinkUpdateReducer();
    this.setState = () => {};
  }

  async loadLinks(json) {
    // if initial contacts, queue up getting these paths from link-store
    let data = _.get(json, 'group-initial', false);
    if (data) {
      for (let each of Object.keys(data)) {
        let linkUrl = "/~link/submissions" + each + ".json?p=0";
        let promise = await fetch(linkUrl);
        if (promise.ok) {
          let resolvedData = {}
          resolvedData.link = {};
          resolvedData.link[each] = {};
          resolvedData.link[each] = await promise.json();
          this.handleEvent(resolvedData);
        }
      }
    }
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

    console.log(json);
    this.loadLinks(json);
    this.initialReducer.reduce(json, this.state);
    this.permissionUpdateReducer.reduce(json, this.state);
    this.localReducer.reduce(json, this.state);
    this.linkUpdateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
