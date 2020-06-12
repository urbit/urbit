import GraphReducer from '../reducers/graph-update';

import BaseStore from './base';


export default class GraphStore extends BaseStore {
  constructor() {
    super();
    this.graphReducer = new GraphReducer();
  }

  initialState() {
    return {
      keys: [],
      graphs: {},
      sidebarShown: true,
    };
  }

  reduce(data, state) {
    this.graphReducer.reduce(data, this.state);
  }
}

