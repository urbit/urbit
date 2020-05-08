import { InitialReducer } from '/reducers/initial';
import { LocalReducer } from '/reducers/local.js';
import _ from 'lodash';


class Store {
  constructor() {
    this.state = {
      status: null,
      apps: {},
      threads: {},
      peers: { known: [], alien: [], deets: {}},
      timers: [],
      commits: [],
      bindings: [],
      sidebarShown: true
    };

    this.initialReducer = new InitialReducer();
    this.localReducer = new LocalReducer();
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
    this.localReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
