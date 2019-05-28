import _ from 'lodash';
import { ChatReducer } from '/reducers/chat';
import { UpdateReducer } from '/reducers/update';


class Store {
  constructor() {
    this.state = {
      inbox: {},
      messages: [],
      configs: {},
      circles: []
    };

    this.chatReducer = new ChatReducer();
    this.updateReducer = new UpdateReducer();
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    this.chatReducer.reduce(json, this.state);
    this.updateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();

