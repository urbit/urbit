import _ from 'lodash';
import { BigIntOrderedMap } from "~/logic/lib/BigIntOrderedMap";
import bigInt, { BigInteger } from "big-integer";
import useGraphState, { GraphState } from '../state/graph';
import { reduceState } from '../state/base';

export const GraphReducer = (json) => {
  const data = _.get(json, 'graph-update', false);
  
  if (data) {
    reduceState<GraphState, any>(useGraphState, data, [
      keys,
      addGraph,
      removeGraph,
      addNodes,
      removeNodes
    ]);
  }
};

const keys = (json, state: GraphState): GraphState => {
  const data = _.get(json, 'keys', false);
  if (data) {
    state.graphKeys = new Set(data.map((res) => {
      let resource = res.ship + '/' + res.name;
      return resource;
    }));
  }
  return state;
};

const addGraph = (json, state: GraphState): GraphState => {

  const _processNode = (node) => {
    //  is empty
    if (!node.children) {
      node.children = new BigIntOrderedMap();
      return node;
    }

    //  is graph
    let converted = new BigIntOrderedMap();
    for (let idx in node.children) {
      let item = node.children[idx];
      let index = bigInt(idx);
      
      converted.set(
        index,
        _processNode(item)
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
    state.graphTimesentMap[resource] = {};


    for (let idx in data.graph) {
      let item = data.graph[idx];
      let index = bigInt(idx);
      
      let node = _processNode(item);

      state.graphs[resource].set(
        index,
        node
      );
    }
    state.graphKeys.add(resource);
  }
  return state;
};

const removeGraph = (json, state: GraphState): GraphState => {
  const data = _.get(json, 'remove-graph', false);
  if (data) {

    if (!('graphs' in state)) {
      state.graphs = {};
    }
    let resource = data.ship + '/' + data.name;
    state.graphKeys.delete(resource);
    delete state.graphs[resource];
  }
  return state;
};

const mapifyChildren = (children) => {
  return new BigIntOrderedMap(
    _.map(children, (node, idx) => {
      idx = idx && idx.startsWith('/') ? idx.slice(1) : idx;
      const nd = {...node, children: mapifyChildren(node.children || {}) }; 
      return [bigInt(idx), nd];
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

  const _remove = (graph, index) => {
    if (index.length === 1) {
      graph.delete(index[0]);
    } else {
      const child = graph.get(index[0]);
      if (child) {
        child.children = _remove(child.children, index.slice(1));
        graph.set(index[0], child);
      }
    }

    return graph;
  };

  const _killByFuzzyTimestamp = (graph, resource, timestamp) => {
    if (state.graphTimesentMap[resource][timestamp]) {
      let index = state.graphTimesentMap[resource][timestamp];

      if (index.split('/').length === 0) { return; }
      let indexArr = index.split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });

      graph = _remove(graph, indexArr);
      delete state.graphTimesentMap[resource][timestamp];
    }

    return graph;
  };

  const _removePending = (graph, post, resource) => {
    if (!post.hash) {
      return graph;
    }

    graph = _killByFuzzyTimestamp(graph, resource, post['time-sent']);
    graph = _killByFuzzyTimestamp(graph, resource, post['time-sent'] - 1);
    graph = _killByFuzzyTimestamp(graph, resource, post['time-sent'] + 1);

    return graph;
  };

  const data = _.get(json, 'add-nodes', false);
  if (data) {
    if (!('graphs' in state)) { return state; }

    let resource = data.resource.ship + '/' + data.resource.name;
    if (!(resource in state.graphs)) { 
      state.graphs[resource] = new BigIntOrderedMap();
    }

    if (!(resource in state.graphTimesentMap)) {
      state.graphTimesentMap[resource] = {};
    }

    state.graphKeys.add(resource);
    
    let indices = Array.from(Object.keys(data.nodes));

    indices.sort((a, b) => {
      let aArr = a.split('/');
      let bArr = b.split('/');
      return bArr.length < aArr.length;
    });

    let graph = state.graphs[resource];

    indices.forEach((index) => {
      let node = data.nodes[index];
      graph = _removePending(graph, node.post, resource);
      
      if (index.split('/').length === 0) { return; }
      let indexArr = index.split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });

      if (indexArr.length === 0) { return state; }

      if (node.post.pending) {
        state.graphTimesentMap[resource][node.post['time-sent']] = index;
      }

      node.children = mapifyChildren(node?.children || {});
     
      graph = _addNode(
        graph,
        indexArr,
        node
      );
      
    });

    state.graphs[resource] = graph;
  }
  return state;
};

const removeNodes = (json, state: GraphState): GraphState => {
  const _remove = (graph, index) => {
    if (index.length === 1) {
        graph.delete(index[0]);
      } else {
        const child = graph.get(index[0]);
        if (child) {
          _remove(child.children, index.slice(1));
          graph.set(index[0], child);
        }
      }
  };

  const data = _.get(json, 'remove-nodes', false);
  if (data) {
    const { ship, name } = data.resource;
    const res = `${ship}/${name}`;
    if (!(res in state.graphs)) { return state; }

    data.indices.forEach((index) => {
      if (index.split('/').length === 0) { return; }
      let indexArr = index.split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });
      _remove(state.graphs[res], indexArr);
    });
  }
  return state;
};
