import { InitialReducer } from '/reducers/initial';
import { GroupUpdateReducer } from '/reducers/group-update';
import { InboxUpdateReducer } from '/reducers/inbox-update';



class Store {
  constructor() {
    this.state = {
      inbox: {},
      groups: {}
    };

    this.initialReducer = new InitialReducer();
    this.groupUpdateReducer = new GroupUpdateReducer();
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
    this.inboxUpdateReducer.reduce(json, this.state);

    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
