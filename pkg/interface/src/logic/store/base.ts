export default class BaseStore<S extends object> {
  state: S;
  setState: (s: Partial<S>) => void = (s) => {};
  constructor() {
    this.state = this.initialState();
  }

  initialState() {
    return {} as S;
  }

  setStateHandler(setState: (s: Partial<S>) => void) {
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
