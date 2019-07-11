import { InitialReducer } from '/reducers/initial';
import { ConfigReducer } from '/reducers/config';
import { UpdateReducer } from '/reducers/update';


class Store {
  constructor() {
    this.state = {
      inbox: {},
      messages: {},
      configs: {},
      circles: [],
      peers: {},
      spinner: false
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
  }
}

export let store = new Store();
window.store = store;
