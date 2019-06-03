import _ from 'lodash';
import { InitialReducer } from '/reducers/initial';
import { ConfigReducer } from '/reducers/config';
import { UpdateReducer } from '/reducers/update';


class Store {
  constructor() {
    //let state = localStorage.getItem('store');

    let state = false;
    if (!state) {
      this.state = {
        inbox: {},
        messages: [],
        configs: {},
        circles: [],
        peers: {},
        local: false
      };
    } else {
      this.state = JSON.parse(state);
      // TODO: wtf???
      delete this.state.messages[undefined];
      console.log(this.state);
      this.state.local = true;
    }

    this.initialReducer = new InitialReducer();
    this.configReducer = new ConfigReducer();
    this.updateReducer = new UpdateReducer();
    this.setState = () => {};

  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    console.log(data);
    let json = data.data;

    this.initialReducer.reduce(json, this.state);
    this.configReducer.reduce(json, this.state);
    this.updateReducer.reduce(json, this.state);

    this.setState(this.state);
    //localStorage.setItem('store', JSON.stringify(this.state));
  }
}

export let store = new Store();
window.store = store;
