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
    this.state.status = data.data.status;

    if (data.data.status !== "pending"){
      this.state.data = data.data.data;
    }

    this.state.current = data.data.current;
    this.state.sessions = data.data.sessions;
    this.setState(this.state);
//    console.log("store", this.state);
  }

}

export let store = new Store();
window.store = store;
