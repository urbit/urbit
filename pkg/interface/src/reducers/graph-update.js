import _ from 'lodash';

export default class GraphReducer {
  reduce(json, state) {
    const data = _.get(json, 'graph-update', false);
    if (data) {
      this.keys(data, state);
      this.addGraph(data, state);
    }
  }

  keys(json, state) {
    const data = _.get(json, 'keys', false);
    if (data) {
      state.keys = data.map((res) => {
        return res.ship + '/' + res.name;
      });
    }
  }

  addGraph(json, state) {
    const data = _.get(json, 'add-graph', false);
    if (data) {
      if (!('graphs' in state)) {
        state.graphs = {};
      }
      let resource = data.resource.ship + '/' + data.resource.name;
      state.graphs[resource] = data.graph;
    }
  }

  removeGraph(json, state) {
    const data = _.get(json, 'remove-graph', false);
    if (data) {
      if (!('graphs' in state)) {
        state.graphs = {};
      }
      let resource = data.resource.ship + '/' + data.resource.name;
      delete state.graphs[resource];
    }
  }

}
