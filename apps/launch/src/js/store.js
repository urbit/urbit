
class Store {
  constructor() {
    this.state = {};
    this.setState = () => {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  handleEvent(data) {
    let json = data.data;

    this.setState(json);
  }
}

export let store = new Store();
window.store = store;
