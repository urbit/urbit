import BaseStore from './base';
import LaunchReducer from '../reducers/launch-update';

export default class LaunchStore extends BaseStore {
  constructor() {
    super();
    this.launchReducer = new LaunchReducer();
  }

  reduce(data, state) {
    this.launchReducer.reduce(data, state);
  }
}
