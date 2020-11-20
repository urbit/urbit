import _ from 'lodash';
import { BigIntOrderedMap } from "~/logic/lib/BigIntOrderedMap";
import bigInt, { BigInteger } from "big-integer";

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
      return resource;
    }));
  }
};

const addGraph = (json, state) => {

  const _processNode = (node) => {
    //  is empty
    if (!node.children) {
      node.children = new BigIntOrderedMap();
      return node;
    }

    //  is graph
    let converted = new BigIntOrderedMap();
    for (let i in node.children) {
      let item = node.children[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return bigInt(ind);
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
    state.graphs[resource] = new BigIntOrderedMap();

    for (let i in data.graph) {
      let item = data.graph[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });

      if (index.length === 0) { break; }
      
      let node = _processNode(item[1]);

      state.graphs[resource].set(
        index[index.length - 1],
        node
      );
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
    let resource = data.ship + '/' + data.name;
    delete state.graphs[resource];
  }
};

const mapifyChildren = (children) => {
  return new BigIntOrderedMap(
    children.map(([idx, node]) => {
      const nd = {...node, children: mapifyChildren(node.children || []) }; 
      return [bigInt(idx.slice(1)), nd];
    }));
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
        return bigInt(ind);
      });

      if (index.length === 0) { return; }

      item[1].children = mapifyChildren(item[1].children || []);


      
      state.graphs[resource] = _addNode(
        state.graphs[resource],
        index,
        item[1]
      );
    }
  }
};

const removeNodes = (json, state) => {
  const _remove = (graph, index) => {
    if (index.length === 1) {
        graph.delete(index[0]);
      } else {
        const child = graph.get(index[0]);
        _remove(child.children, index.slice(1));
        graph.set(index[0], child);
      }
  };
  const data = _.get(json, 'remove-nodes', false);
  if (data) {
    const { ship, name } = data.resource;
    const res = `${ship}/${name}`;
    if (!(res in state.graphs)) { return; }

    data.indices.forEach((index) => {
      if (index.split('/').length === 0) { return; }
      let indexArr = index.split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });
      _remove(state.graphs[res], indexArr);
    });
  }
};
