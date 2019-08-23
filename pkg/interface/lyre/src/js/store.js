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
    this.state.body = data.data.body;
    this.state.current = data.data.current;
    this.state.sessions = data.data.sessions;
    this.setState(this.state);
    console.log("state", this.state);
  }

}

export let store = new Store();
window.store = store;
