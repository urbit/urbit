export default class BaseStore {
  constructor() {
    this.state = this.initialState();
    this.setState = () => {};
  }

  initialState() {
    return {};
  }

  setStateHandler(setState) {
    this.setState = setState;
  }

  clear() {
    this.handleEvent({
      data: { clear: true }
    });
  }

  handleEvent(data) {
    const json = data.data;

    if (json === null) {
      return;
    }

    if ('clear' in json && json.clear) {
      this.setState(this.initialState());
      return;
    }

    this.reduce(json, this.state);
    this.setState(this.state);
  }

  reduce(data, state) {
    // extend me!
  }
}

