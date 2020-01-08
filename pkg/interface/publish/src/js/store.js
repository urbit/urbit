import { InitialReducer }  from '/reducers/initial';
import { PrimaryReducer }  from '/reducers/primary';
import { ResponseReducer } from '/reducers/response';

class Store {
  constructor() {
    this.state = {
      notebooks: {},
      groups: {},
      permissions: {},
      invites: {},
      spinner: false,
      sidebarShown: false,
    }

    this.initialReducer  = new InitialReducer();
    this.primaryReducer  = new PrimaryReducer();
    this.responseReducer = new ResponseReducer();
    this.setState = () => {};

    this.initialReducer.reduce(window.injectedState, this.state);
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(evt) {
    if (evt.from && evt.from.path === '/primary'){
      this.primaryReducer.reduce(evt.data, this.state);
    } else if (evt.type) {
      this.responseReducer.reduce(evt, this.state);
    }
    this.setState(this.state);
  }
}

export let store = new Store();
window.store = store;
