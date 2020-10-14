import _ from 'lodash';


export const GraphReducer = (json, state) => {
  const data = _.get(json, 'graph-update', false);
  if (data) {
    keys(data, state);
    addGraph(data, state);
    removeGraph(data, state);
    addNodes(data, state);
    removeNodes(data, state);
  }
};

const keys = (json, state) => {
  const data = _.get(json, 'keys', false);
  if (data) {
    state.graphKeys = new Set(data.map((res) => {
      let resource = res.ship + '/' + res.name;

      if (!(resource in state.graphs)) {
        state.graphs[resource] = new Map();
      }

      return resource;
    }));
  }
};

const addGraph = (json, state) => {

  const _processNode = (node) => {
    //  is empty
    if (!node.children) {
      node.children = new Map();
      return node;
    }

    //  is graph
    let converted = new Map();
    for (let i in node.children) {
      let item = node.children[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return parseInt(ind, 10);
      });

      if (index.length === 0) { break; }
      
      converted.set(
        index[index.length - 1],
        _processNode(item[1])
      );
    }
    node.children = converted;
    return node;
  };

  const data = _.get(json, 'add-graph', false);
  if (data) {
    if (!('graphs' in state)) {
      state.graphs = {};
    }

    let resource = data.resource.ship + '/' + data.resource.name;
    state.graphs[resource] = new Map();

    for (let i in data.graph) {
      let item = data.graph[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return parseInt(ind, 10);
      });

      if (index.length === 0) { break; }
      
      let node = _processNode(item[1]);
      state.graphs[resource].set(index[index.length - 1], node);
    }
    state.graphKeys.add(resource);
  }

};

const removeGraph = (json, state) => {
  const data = _.get(json, 'remove-graph', false);
  if (data) {
    if (!('graphs' in state)) {
      state.graphs = {};
    }
    let resource = data.resource.ship + '/' + data.resource.name;
    delete state.graphs[resource];
  }
};

const addNodes = (json, state) => {
  const _addNode = (graph, index, node) => {
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
    parNode.children = _addNode(parNode.children, index.slice(1), node);
    graph.set(index[0], parNode);
    return graph;
  };

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

      //  TODO: support adding nodes with children
      item[1].children = new Map();
      
      state.graphs[resource] = _addNode(
        state.graphs[resource],
        index,
        item[1]
      );
    }
  }
};

const removeNodes = (json, state) => {
  const data = _.get(json, 'remove-nodes', false);
  if (data) {
    console.log(data);
    if (!(data.resource in state.graphs)) { return; }

    data.indices.forEach((index) => {
      console.log(index);
      if (index.split('/').length === 0) { return; }
      let indexArr = index.split('/').slice(1).map((ind) => {
        return parseInt(ind, 10);
      });

      if (indexArr.length === 1) {
        state.graphs[data.resource].delete(indexArr[0]);
      } else {
        // TODO: recursive
      }

    });
  }
};

