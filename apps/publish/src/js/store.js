import { UpdateReducer } from '/reducers/update';
import { RumorReducer } from '/reducers/rumor';

class Store {
  constructor() {
    this.state = {
      ...window.injectedState,
    }
    console.log("store.state", this.state);

    this.updateReducer = new UpdateReducer();
    this.rumorReducer  = new RumorReducer();

    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    console.log("store.handleEvent", data);
    this.updateReducer.reduce(data.data, this.state);
    this.rumorReducer.reduce(data.data, this.state);
    this.setState(this.state);
  }

}

export let store = new Store();
window.store = store;
