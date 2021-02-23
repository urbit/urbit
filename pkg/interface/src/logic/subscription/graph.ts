import bigInt from "big-integer";
import _ from "lodash";
import { compose } from "lodash/fp";

import { Graph, GraphNode, GraphUpdate } from "@urbit/api";
import BigIntOrderedMap from "@urbit/api/lib/BigIntOrderedMap";
import { SubscriptionRequestInterface, UrbitInterface } from "@urbit/http-api";

import useGraphState, { GraphState } from "~/logic/state/graph";

export const keys = (json: GraphUpdate, state: GraphState): GraphState => {
  const data = _.get(json, 'keys', false);
  if (data) {
    state.graphKeys = new Set(data.map((res) => {
      let resource = res.ship + '/' + res.name;
      return resource;
    }));
  }
  return state;
};

export const addGraph = (json: GraphUpdate, state: GraphState): GraphState => {
  const _processNode = (node: GraphNode): GraphNode => {
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

    for (let idx in data.graph) {
      let item = data.graph[idx];
      let index = bigInt(idx);
      
      let node = _processNode(item);

      state.graphs[resource].set(
        index,
        node
      );
    }
    console.log('graphkeys', state);
    state.graphKeys.add(resource);
  }
  return state;
};


export const removeGraph = (json: GraphUpdate, state: GraphState): GraphState => {
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
}

export const mapifyChildren = (children): BigIntOrderedMap<GraphNode> => {
  return new BigIntOrderedMap(
    _.map(children, (node, idx) => {
      idx = idx && idx.startsWith('/') ? idx.slice(1) : idx;
      const nd = {...node, children: mapifyChildren(node.children || {}) }; 
      return [bigInt(idx), nd];
    }));
};

export const addNodes = (json: GraphUpdate, state: GraphState): GraphState => {
  const _addNode = (
    graph: Graph,
    index,
    node: GraphNode
  ): Graph => {
    //  set child of graph
    if (index.length === 1) {
      graph.set(index[0], node);
      return graph;
    }

    // set parent of graph
    let parNode = graph.get(index[0]);
    if (!parNode) {
      console.error('parent node does not exist, cannot add child');
      return graph;
    }
    parNode.children = _addNode(parNode.children, index.slice(1), node);
    graph.set(index[0], parNode);
    return graph;
  };

  const data = _.get(json, 'add-nodes', false);
  if (data) {
    if (!('graphs' in state)) {
      return state;
    }

    let resource = data.resource.ship + '/' + data.resource.name;
    if (!(resource in state.graphs)) { 
      state.graphs[resource] = new BigIntOrderedMap();
    }
    state.graphKeys.add(resource);

    for (let index in data.nodes) {
      let node = data.nodes[index];
      if (index.split('/').length === 0) {
        return state;
      }

      index = index.split('/').slice(1).map((ind) => {
        return bigInt(ind);
      });

      if (index.length === 0) {
        return state;
      }

      node.children = mapifyChildren(node?.children || {});

      state.graphs[resource] = _addNode(
        state.graphs[resource],
        index,
        node
      );
    }
  }
  return state;
};

export const removeNodes = (json: GraphUpdate, state: GraphState): GraphState => {
  const _remove = (graph: Graph, index) => {
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
    if (!(res in state.graphs)) {
      return state;
    }

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

export const graphReducer = (message) => {
  useGraphState.setState(
    compose([
      keys,
      addGraph,
      removeGraph,
      addNodes,
      removeNodes
    ].map(reducer => reducer.bind(reducer, message['graph-update']))
    )(useGraphState.getState())
  );
}
const graphSubscription = (channel: UrbitInterface): SubscriptionRequestInterface => {
  const event = graphReducer;
  const err = (message) => {
    console.error(message);
    channel.subscribe(graphSubscription(channel));
  };
  const quit = (message) => {
    console.error(message);
    channel.subscribe(graphSubscription(channel));
  };
  return {
    app: 'graph-store',
    path: '/updates',
    event, err, quit
  };
};

export default graphSubscription;