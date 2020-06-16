import _ from 'lodash';


const OrderedMap = (arr = []) => {
  let map = new Map(arr);
//  map[Symbol.iterator] = function* () {
//      yield* [...this.entries()].sort((a, b) => a[1] - b[1]);
//  };
  return map;
};

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
      if (!(resource in state.graphs)) {
        state.graphs[resource] = new OrderedMap();
      }

      for (let i in data.graph) {
        let item = data.graph[i];
        let index = item[0].split('/').slice(1).map((ind) => {
          return parseInt(ind, 10);
        });

        if (index.length === 0) { break; }
        
        let node = this._processNode(item[1]);
        state.graphs[resource].set(index[index.length - 1], node);
      }
      state.keys.add(resource);
    }
  }

  _processNode(node) {
    //  is empty
    if (!node.children) {
      node.children = new OrderedMap();
      return node;
    }

    //  is graph
    let converted = new OrderedMap();
    for (let i in node.children) {
      let item = node.children[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return parseInt(ind, 10);
      });

      if (index.length === 0) { break; }
      
      let node = this._processNode(item[1]);
      converted.set(index[index.length - 1], node);
    }
    node.children = converted;
    return node;
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
      if (!(resource in state.graphs)) { return; }

      for (let i in data.nodes) {
        let item = data.nodes[i];
        if (item[0].split('/').length === 0) { return; }

        let index = item[0].split('/').slice(1).map((ind) => {
          return parseInt(ind, 10);
        });

        if (index.length === 0) { return; }
        
        state.graphs[resource] = this._addNode(
          state.graphs[resource],
          index,
          item[1]
        );
      }
    }
  }

  //  TODO: recursive add node
  _addNode(graph, index, node) {
    console.log(index, node);
    //  set child of graph
    if (index.length === 1) {
      graph.set(index[0], node);
      return graph;
    }

    // set parent of graph
    let parNode = graph.get(index[0]);
    if (!parNode) {
      console.error('parent node does not exist, cannot add child');
      return;
    }
    parNode.children = this._addNode(parNode.children, index.slice(1), node);
    graph.set(index[0], parNode);
    return graph;
  }
}
