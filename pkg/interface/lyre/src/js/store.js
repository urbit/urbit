class Store {
  constructor() {
    this.state = {
      ...window.injectedState,
    }
    
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    this.state.path = data.data.path;
    this.state.current = data.data.current;
    this.setState(this.state);
    console.log("state", this.state);
  }

}

export let store = new Store();
window.store = store;
