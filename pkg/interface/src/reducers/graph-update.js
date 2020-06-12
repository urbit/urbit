import _ from 'lodash';

export default class GraphReducer {
  reduce(json, state) {
    const data = _.get(json, 'graph-update', false);
    if (data) {
      this.keys(data, state);
      this.addGraph(data, state);
      this.addNodes(data, state);
    }
  }

  keys(json, state) {
    const data = _.get(json, 'keys', false);
    if (data) {
      state.keys = new Set(data.map((res) => {
        return res.ship + '/' + res.name;
      }));
    }
  }

  addGraph(json, state) {
    const data = _.get(json, 'add-graph', false);
    if (data) {
      if (!('graphs' in state)) {
        state.graphs = {};
      }
      let resource = data.resource.ship + '/' + data.resource.name;
      state.graphs[resource] = new Map(data.graph);
      state.keys.add(resource);
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

  addNodes(json, state) {
    const data = _.get(json, 'add-nodes', false);
    if (data) {
      if (!('graphs' in state)) { return; }
      let resource = data.resource.ship + '/' + data.resource.name;
      if (!(resource in state.graphs)) {
        return;
      }
      let graph = state.graphs[resource];
      for (let node in data.nodes) {
        console.log(node);
      }
      state.graphs[resource] = graph;
    }
  }

}
