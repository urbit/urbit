class Store {
  constructor() {
    this.state = window.injectedState;
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    console.log("store.handleEvent", data);
  }

}

export let store = new Store();
window.store = store;
