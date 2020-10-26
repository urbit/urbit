import _ from 'lodash';
import { OrderedMap } from "~/logic/lib/OrderedMap";

const DA_UNIX_EPOCH = 170141184475152167957503069145530368000;
const normalizeKey = (key) => {
  if(key > DA_UNIX_EPOCH) {
    // new links uses milliseconds since unix epoch
    // old (pre-graph-store) use @da
    // ported from +time:enjs:format in hoon.hoon
    return Math.round((1000 * (9223372036854775 + (key - DA_UNIX_EPOCH))) / 18446744073709551616);
  }
  return key;
}

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
      node.children = new OrderedMap();
      node.post.originalIndex = node.post.index;
      node.post.index = node.post.index.split('/').map(x => x.length === 0 ? '' : normalizeKey(parseInt(x, 10))).join('/');
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

      const normalKey = normalizeKey(index[index.length - 1]);
      item[1].post.originalKey = index[index.length - 1];
      
      converted.set(
        normalKey,
        _processNode(item[1])
      );
    }
    node.children = converted;
    node.post.originalIndex = node.post.index;
    node.post.index = node.post.index.split('/').map(x => x.length === 0 ? '' : normalizeKey(parseInt(x, 10))).join('/');
    return node;
  };

  const data = _.get(json, 'add-graph', false);
  if (data) {
    if (!('graphs' in state)) {
      state.graphs = {};
    }

    let resource = data.resource.ship + '/' + data.resource.name;
    state.graphs[resource] = new OrderedMap();

    for (let i in data.graph) {
      let item = data.graph[i];
      let index = item[0].split('/').slice(1).map((ind) => {
        return parseInt(ind, 10);
      });

      if (index.length === 0) { break; }
      
      let node = _processNode(item[1]);

      const normalKey = normalizeKey(index[index.length - 1])
      node.post.originalKey = index[index.length - 1];
      state.graphs[resource].set(normalKey, node);
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

const mapifyChildren = (children) => {
  return new OrderedMap(
    children.map(([idx, node]) => {
      const nd = {...node, children: mapifyChildren(node.children || []) }; 
      return [normalizeKey(parseInt(idx.slice(1), 10)), nd];
    }));
};

const addNodes = (json, state) => {
  const _addNode = (graph, index, node) => {
    //  set child of graph
    if (index.length === 1) {
      node.post.originalIndex = node.post.index;
      node.post.index = node.post.index.split('/').map(x => x.length === 0 ? '' : normalizeKey(parseInt(x, 10))).join('/');

      const normalKey = normalizeKey(index[0])
      node.post.originalKey = index[0];
      graph.set(normalKey, node);
      return graph;
    }

    // set parent of graph
    let parNode = graph.get(normalizeKey(index[0]));
    if (!parNode) {
      console.error('parent node does not exist, cannot add child');
      return;
    }
    parNode.children = _addNode(parNode.children, index.slice(1), node);
    graph.set(normalizeKey(index[0]), parNode);
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
        const child = graph.get(normalizeKey(index[0]));
        _remove(child.children, index.slice(1));
        graph.set(normalizeKey(index[0]), child);
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
        return parseInt(ind, 10);
      });
      _remove(state.graphs[res], indexArr);
    });
  }
};
