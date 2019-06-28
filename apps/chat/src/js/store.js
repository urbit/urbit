import _ from 'lodash';
import { InitialReducer } from '/reducers/initial';
import { ConfigReducer } from '/reducers/config';
import { UpdateReducer } from '/reducers/update';


class Store {
  constructor() {
    this.start = performance.now();

    this.state = {
      inbox: {},
      messages: {},
      configs: {},
      circles: [],
      peers: {},
      local: false
    };

    this.initialReducer = new InitialReducer();
    this.configReducer = new ConfigReducer();
    this.updateReducer = new UpdateReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    this.initialReducer.reduce(json, this.state);
    this.configReducer.reduce(json, this.state);
    this.updateReducer.reduce(json, this.state);

    this.setState(this.state);

    this.end = performance.now();
    console.log('performance.now(): ', this.end - this.start);
  }
}

export let store = new Store();
window.store = store;
