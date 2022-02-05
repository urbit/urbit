import { arrToString, stringToArr, BigIntOrderedMap, BigIntArrayOrderedMap, GraphNode } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import produce from 'immer';
import _ from 'lodash';
import { BaseState, reduceState } from '../state/base';
import useGraphState, { GraphState as State } from '../state/graph';
/* eslint-disable camelcase */

import { unstable_batchedUpdates } from 'react-dom';

type GraphState = State & BaseState<State>;

const mapifyChildren = (children) => {
  return new BigIntOrderedMap().gas(
    _.map(children, (node, idx) => {
      idx = idx && idx.startsWith('/') ? idx.slice(1) : idx;
      const nd = { ...node, children: mapifyChildren(node.children || {}) };
      return [bigInt(idx), nd];
    }));
};

const processNode = (node) => {
  //  is empty
  if (!node.children) {
    return produce<GraphNode>(node, (draft: GraphNode) => {
      draft.children = new BigIntOrderedMap();
    });
  }

  //  is graph
  return produce<GraphNode>(node, (draft: GraphNode) => {
    draft.children = new BigIntOrderedMap<GraphNode>()
      .gas(_.map(draft.children, (item, idx) =>
        [bigInt(idx), processNode(item)] as [BigInteger, any]
      ));
  });
};
const acceptOrRejectDm = (json: any, state: GraphState): GraphState => {
  const data = _.get(json, 'accept', _.get(json, 'decline', false));
  if(data) {
    state.pendingDms.delete(data);
  }
  return state;
};

const pendings = (json: any, state: GraphState): GraphState => {
  const data = _.get(json, 'pendings', false);
  if(data) {
    state.pendingDms = new Set(data);
  }
  return state;
};

const setScreen = (json: any, state: GraphState): GraphState => {
  if('screen' in json) {
    state.screening = json.screen;
  }
  return state;
};

const addNodesLoose = (json: any, state: GraphState): GraphState => {
  const data = _.get(json, 'add-nodes', false);
  if(data) {
    const { resource: { ship, name }, nodes } = data;
    const resource = `${ship}/${name}`;

    const indices = _.get(state.looseNodes, [resource], {});
    _.forIn(nodes, (node, index) => {
      indices[index] = processNode(node);
    });
    _.set(state.looseNodes, [resource], indices);
  }
  return state;
};

const addNodesFlat = (json: any, state: GraphState): GraphState => {
  const data = _.get(json, 'add-nodes', false);
  if (data) {
      if (!('flatGraphs' in state)) {
   return state;
  }

    const resource = data.resource.ship + '/' + data.resource.name;
    if (!(resource in state.flatGraphs)) {
      state.flatGraphs[resource] = new BigIntArrayOrderedMap();
    }

    if (!(resource in state.graphTimesentMap)) {
      state.graphTimesentMap[resource] = {};
    }

    const indices = Array.from(Object.keys(data.nodes));

    indices.forEach((index) => {
      if (index.split('/').length === 0) {
       return;
      }
      const indexArr = stringToArr(index);
      if (indexArr.length === 0) {
       return state;
      }

      const node = data.nodes[index];
      node.children = mapifyChildren({});
      state.flatGraphs[resource] =
        state.flatGraphs[resource].set(indexArr, node);
    });
  }
  return state;
};

const addNodesThread = (json: any, state: GraphState): GraphState => {
  const data = _.get(json, 'add-nodes', false);
  const parentIndex = _.get(json, 'index', false);
  if (data && parentIndex) {
    if (!('threadGraphs' in state)) {
      return state;
    }

    const resource = data.resource.ship + '/' + data.resource.name;
    if (!(resource in state.threadGraphs)) {
      state.threadGraphs[resource] = {};
    }

    const indices = Array.from(Object.keys(data.nodes));
    if (!(parentIndex in state.threadGraphs[resource])) {
      state.threadGraphs[resource][parentIndex] = new BigIntArrayOrderedMap([], true);
    }

    indices.forEach((index) => {
      if (index.split('/').length === 0) {
       return;
      }
      const indexArr = stringToArr(index);

      if (indexArr.length === 0) {
       return state;
      }

      const node = data.nodes[index];
      node.children = mapifyChildren({});
      state.threadGraphs[resource][parentIndex] =
        state.threadGraphs[resource][parentIndex].set(indexArr, node);
    });
  }
  return state;
};

const keys = (json, state: GraphState): GraphState => {
  const data = _.get(json, 'keys', false);
  if (data) {
    state.graphKeys = new Set(data.map((res) => {
      const resource = res.ship + '/' + res.name;
      return resource;
    }));
  }
  return state;
};

const addGraph = (json, state: GraphState): GraphState => {
  const data = _.get(json, 'add-graph', false);
  if (data) {
    if (!('graphs' in state)) {
      // @ts-ignore investigate zustand types
      state.graphs = {};
    }

    if (!('flatGraphs' in state)) {
      // @ts-ignore investigate zustand types
      state.flatGraphs = {};
    }

    const resource = data.resource.ship + '/' + data.resource.name;
    state.graphs[resource] = new BigIntOrderedMap();
    state.graphTimesentMap[resource] = {};

    state.graphs[resource] = state.graphs[resource].gas(Object.keys(data.graph).map((idx) => {
      return [bigInt(idx), processNode(data.graph[idx])];
    }));

    state.graphKeys.add(resource);
  }
  return state;
};

const removeGraph = (json, state: GraphState): GraphState => {
  const data = _.get(json, 'remove-graph', false);
  if (data) {
    if (!('graphs' in state)) {
      // @ts-ignore investigate zustand types
      state.graphs = {};
    }

    if (!('graphs' in state)) {
      // @ts-ignore investigate zustand types
      state.flatGraphs = {};
    }
    const resource = data.ship + '/' + data.name;
    state.graphKeys.delete(resource);
    delete state.graphs[resource];
  }
  return state;
};

export const addNodes = (json, state) => {
  const _addNode = (graph, index, node) => {
    //  set child of graph
    if (index.length === 1) {
      if (graph.has(index[0])) {
        return graph;
      }
      return graph.set(index[0], node);
    }

    // set parent of graph
    const parNode = graph.get(index[0]);
    if (!parNode) {
      console.error('parent node does not exist, cannot add child');
      return graph;
    }
    return graph.set(index[0], produce(parNode, (draft) => {
      draft.children = _addNode(draft.children, index.slice(1), node);
    }));
  };

  const _remove = (graph, index) => {
    if (index.length === 1) {
      return graph.delete(index[0]);
    } else {
      const child = graph.get(index[0]);
      if (child) {
        return graph.set(index[0], produce(child, (draft) => {
          draft.children = _remove(draft.children, index.slice(1));
        }));
      }
    }

    return graph;
  };

  const _removePending = (
    graph,
    flatGraph,
    threadGraphs,
    post,
    resource
  ) => {
    const timestamp = post['time-sent'];

    if (state.graphTimesentMap[resource][timestamp]) {
      const index = state.graphTimesentMap[resource][timestamp];

      if (index.split('/').length === 0) {
       return graph;
      }
      const indexArr = stringToArr(index);

      delete state.graphTimesentMap[resource][timestamp];

      if (graph) {
        graph = _remove(graph, indexArr);
      }

      if (flatGraph) {
        flatGraph = flatGraph.delete(indexArr);
      }

      if (threadGraphs) {
        const k = [];
        for (const i in indexArr) {
          k.push(indexArr[i]);
          const arr = arrToString(k);
          if (threadGraphs[arr]) {
            threadGraphs[arr] = threadGraphs[arr].delete(indexArr);
          }
        }
      }
    }

    return [graph, flatGraph, threadGraphs];
  };

  const addOrRemoveSigs = _.get(json, 'add-signatures', false) || _.get(json, 'remove-signatures', false);
  console.log(0, addOrRemoveSigs)
  if (addOrRemoveSigs) {
    if (!('graphs' in state) || !('flatGraphs' in state) || !('threadGraphs' in state)) {
      return state;
    }

    const { signatures, uid: { index, resource: { name, ship } } } = addOrRemoveSigs;
    const resource = `${ship}/${name}`;

    if (!(resource in state.graphs)) {
      if(json.fetch) {
        state.graphs[resource] = new BigIntOrderedMap();
      } else {
        return state;
      }
    }

    const indexArr = stringToArr(index);

    const graph = state.graphs[resource];
    const node = graph.get(indexArr[0]);
    const newNode = {
      ...node,
      post: {
        ...node.post,
        signatures: [...node.post.signatures, ...signatures]
      }
    };
    const updatedGraph = graph.set(indexArr[0], newNode);
    state.graphs[resource] = updatedGraph;

    return state;
  }

  const data = _.get(json, 'add-nodes', false);
  if (data) {
    if (!('graphs' in state)) {
     return state;
    }
    if (!('flatGraphs' in state)) {
     return state;
    }
    if (!('threadGraphs' in state)) {
     return state;
    }

    const resource = data.resource.ship + '/' + data.resource.name;
    if (!(resource in state.graphs)) {
      if(json.fetch) {
        state.graphs[resource] = new BigIntOrderedMap();
      } else {
        //  ignore updates until we load backlog deliberately, to avoid
        //  unnecessary memory usage
        return state;
      }
    }

    if (!(resource in state.graphTimesentMap)) {
      state.graphTimesentMap[resource] = {};
    }

    state.graphKeys.add(resource);

    const indices = Array.from(Object.keys(data.nodes));

    indices.sort((a, b) => {
      const aArr = a.split('/');
      const bArr = b.split('/');
      return aArr.length - bArr.length;
    });

    indices.forEach((index) => {
      const node = data.nodes[index];
      const old = state.graphs[resource].size;

      if (index.split('/').length === 0) {
       return state;
      }
      const indexArr = stringToArr(index);

      const [graph, flatGraph, threadGraphs] =
        _removePending(
          state.graphs[resource],
          state.flatGraphs[resource],
          state.threadGraphs[resource],
          node.post,
          resource
        );

      if (graph) {
        state.graphs[resource] = graph;
      }
      if (flatGraph) {
        state.flatGraphs[resource] = flatGraph;
      }
      if (threadGraphs) {
        state.threadGraphs[resource] = threadGraphs;
      }

      const newSize = state.graphs[resource].size;

      if (node.post.pending) {
        state.graphTimesentMap[resource][node.post['time-sent']] = index;
      }

      state.graphs[resource] = _addNode(
        state.graphs[resource],
        indexArr,
        produce(node, (draft) => {
          draft.children = mapifyChildren(draft?.children || {});
        })
      );

      if (resource in state.flatGraphs) {
        state.flatGraphs[resource] =
          state.flatGraphs[resource].set(indexArr, produce(node, (draft) => {
            draft.children = mapifyChildren({});
          }));
      }

      if (indexArr.length > 1 && resource in state.threadGraphs) {
        const parentKey = arrToString(indexArr.slice(0, indexArr.length - 1));

        if (parentKey in state.threadGraphs[resource]) {
          const thread = state.threadGraphs[resource][parentKey];
          const isFirstChild =  Array.from(thread.keys()).filter((k) => {
            //  @ts-ignore @tacryt-socryp what do?
            return stringToArr(parentKey).length < k.length;
          }).length === 0;

          if (isFirstChild) {
            state.threadGraphs[resource][parentKey] =
              state.threadGraphs[resource][parentKey].set(
                indexArr,
                produce(node, (draft) => {
                  draft.children = mapifyChildren({});
                })
              );
          }
        }
      }

      if(newSize !== old) {
        console.log(`${resource}, (${old}, ${newSize}, ${state.graphs[resource].size})`);
      }
    });
  }

  return state;
};

const removePosts = (json, state: GraphState): GraphState => {
  const _remove = (graph, index) => {
    const child = graph.get(index[0]);
    if(!child) {
      return graph;
    }
    if (index.length === 1) {
      return graph.set(index[0], {
        post: child.post.hash || '',
        children: child.children
      });
    } else {
      const node = { ...child, children: _remove(child.children, index.slice(1)) };
      return graph.set(index[0], node);
    }
  };

  const data = _.get(json, 'remove-posts', false);
  if (data) {
    const { ship, name } = data.resource;
    const res = `${ship}/${name}`;
    if (!(res in state.graphs)) {
 return state;
}

    data.indices.forEach((index) => {
      if (index.split('/').length === 0) {
 return;
}
      const indexArr = stringToArr(index);
      state.graphs[res] = _remove(state.graphs[res], indexArr);
    });
  }
  return state;
};

export const reduceDm = [
  acceptOrRejectDm,
  pendings,
  setScreen
];

export const GraphReducer = (json) => {
  const data = _.get(json, 'graph-update', false);

  unstable_batchedUpdates(() => {
    if (data) {
      reduceState<GraphState, any>(useGraphState, data, [
        keys,
        addGraph,
        removeGraph,
        addNodes,
        removePosts
      ]);
    }
    const loose = _.get(json, 'graph-update-loose', false);
    if(loose) {
      reduceState<GraphState, any>(useGraphState, loose, [addNodesLoose]);
    }

    const flat = _.get(json, 'graph-update-flat', false);
    if (flat) {
      reduceState<GraphState, any>(useGraphState, flat, [addNodesFlat]);
    }

    const thread = _.get(json, 'graph-update-thread', false);
    if (thread) {
      reduceState<GraphState, any>(useGraphState, thread, [addNodesThread]);
    }
  });
};
