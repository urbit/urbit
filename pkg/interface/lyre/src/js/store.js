class Store {
  constructor() {
    this.state = {
      path: window.injectedState,
    }
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    this.state.path = data.data;
    this.setState(this.state);
    console.log("state", this.state);
  }

}

export let store = new Store();
window.store = store;
