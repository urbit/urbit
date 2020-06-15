import BaseStore from './base';
import LaunchReducer from '../reducers/launch-update';

export default class LaunchStore extends BaseStore {
  constructor() {
    super();
    this.launchReducer = new LaunchReducer();
  }

  initialState() {
    return {
      launch: {
        firstTime: false,
        tileOrdering: [],
        tiles: {},
      },
      location: '',
      weather: {}
    };
  }

  reduce(data, state) {
    this.launchReducer.reduce(data, state);
  }
}
