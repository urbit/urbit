import { UpdateReducer } from '/reducers/update';
import { RumorReducer } from '/reducers/rumor';
import { SpinnerReducer } from '/reducers/spinner';

class Store {
  constructor() {
    this.state = {
      spinner: false,
      ...window.injectedState,
    }
    this.updateReducer   = new UpdateReducer();
    this.rumorReducer    = new RumorReducer();
    this.spinnerReducer  = new SpinnerReducer();

    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    this.updateReducer.reduce(data.data, this.state);
    this.rumorReducer.reduce(data.data, this.state);
    this.spinnerReducer.reduce(data.data, this.state);
    this.setState(this.state);
  }

}

export let store = new Store();
window.store = store;
