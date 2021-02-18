import _ from 'lodash';
import { PatpNoSig, Patp, Poke, Thread, Path, Enc } from '..';
import { Content, GraphNode, Post, GraphNodePoke, GraphChildrenPoke } from './index.d';
import { deSig, unixToDa } from '../lib/util';
import { makeResource, resourceFromPath } from '../groups/index';
import { GroupPolicy } from '../groups/update.d';

export const createBlankNodeWithChildPost = (
  ship: PatpNoSig,
  parentIndex: string = '',
  childIndex: string = '',
  contents: Content[]
): GraphNodePoke => {
  const date = unixToDa(Date.now()).toString();
  const nodeIndex = parentIndex + '/' + date;

  const childGraph: GraphChildrenPoke = {};
  childGraph[childIndex] = {
    post: {
      author: `~${ship}`,
      index: nodeIndex + '/' + childIndex,
      'time-sent': Date.now(),
      contents,
      hash: null,
      signatures: []
    },
    children: null
  };

  return {
    post: {
      author: `~${ship}`,
      index: nodeIndex,
      'time-sent': Date.now(),
      contents: [],
      hash: null,
      signatures: []
    },
    children: childGraph
  };  
};

function markPending(nodes: any) {
  _.forEach(nodes, node => {
    node.post.author = deSig(node.post.author);
    node.post.pending = true;
    markPending(node.children || {});
  });
}

export const createPost = (
  ship: PatpNoSig,
  contents: Content[],
  parentIndex: string = '',
  childIndex:string = 'DATE_PLACEHOLDER'
): Post => {
  if (childIndex === 'DATE_PLACEHOLDER') {
    childIndex = unixToDa(Date.now()).toString();
  }
  return {
    author: `~${ship}`,
    index: parentIndex + '/' + childIndex,
    'time-sent': Date.now(),
    contents,
    hash: null,
    signatures: []
  };
};

function moduleToMark(mod: string): string | undefined {
  if(mod === 'link') {
    return 'graph-validator-link';
  }
  if(mod === 'publish') {
    return 'graph-validator-publish';
  }
  if(mod === 'chat') {
    return 'graph-validator-chat';
  }
  return undefined;
}

const storeAction = <T>(data: T): Poke<T> => ({
  app: 'graph-store',
  mark: 'graph-update',
  json: data
});

export { storeAction as graphStoreAction };

const viewAction = <T>(threadName: string, action: T): Thread<T> => ({
  inputMark: 'graph-view-action',
  outputMark: 'json',
  threadName,
  body: action
});

export { viewAction as graphViewAction };

const hookAction = <T>(data: T): Poke<T> => ({
  app: 'graph-push-hook',
  mark: 'graph-update',
  json: data
});

export { hookAction as graphHookAction };


export const createManagedGraph = (
  ship: PatpNoSig,
  name: string,
  title: string,
  description: string,
  group: Path,
  mod: string
): Thread<any> => {
  const associated = { group: resourceFromPath(group) };
  const resource = makeResource(`~${ship}`, name);

  return viewAction('graph-create', {
    create: {
      resource,
      title,
      description,
      associated,
      module: mod,
      mark: moduleToMark(mod)
    }
  });
}

export const createUnmanagedGraph = (
  ship: PatpNoSig,
  name: string,
  title: string,
  description: string,
  policy: Enc<GroupPolicy>,
  mod: string
): Thread<any> => {
  const resource = makeResource(`~${ship}`, name);

  return viewAction('graph-create', {
    create: {
      resource,
      title,
      description,
      associated: { policy },
      module: mod,
      mark: moduleToMark(mod)
    }
  });
}

export const joinGraph = (
  ship: Patp,
  name: string
): Thread<any> => {
  const resource = makeResource(ship, name);
  return viewAction('graph-join', {
    join: {
      resource,
      ship,
    }
  });
}

export const deleteGraph = (
  ship: PatpNoSig,
  name: string
): Thread<any> => {
  const resource = makeResource(`~${ship}`, name);
  return viewAction('graph-delete', {
    "delete": {
      resource
    }
  });
}

export const leaveGraph = (
  ship: Patp,
  name: string
): Thread<any> => {
  const resource = makeResource(ship, name);
  return viewAction('graph-leave', {
    "leave": {
      resource
    }
  });
}

export const groupifyGraph = (
  ship: Patp,
  name: string,
  toPath?: string
): Thread<any> => {
  const resource = makeResource(ship, name);
  const to = toPath && resourceFromPath(toPath);

  return viewAction('graph-groupify', {
    groupify: {
      resource,
      to
    }
  });
}

export const evalCord = (
  cord: string
): Thread<any> => {
  return ({
    inputMark: 'graph-view-action',
    outputMark: 'tang',
    threadName: 'graph-eval',
    body: {
      eval: cord
    }
  });
}

export const addGraph = (
  ship: Patp,
  name: string,
  graph: any,
  mark: any
): Poke<any> => {
  return storeAction({
    'add-graph': {
      resource: { ship, name },
      graph,
      mark
    }
  });
}

export const addPost = (
  ship: Patp,
  name: string,
  post: Post
) => {
  let nodes = {};
  nodes[post.index] = {
    post,
    children: null
  };
  return addNodes(ship, name, nodes);
}

export const addNode = (
  ship: Patp,
  name: string,
  node: GraphNode
) => {
  let nodes = {};
  nodes[node.post.index] = node;

  return addNodes(ship, name, nodes);
}

export const addNodes = (
  ship: Patp,
  name: string,
  nodes: Object
): Poke<any> => {
  const action = {
    'add-nodes': {
      resource: { ship, name },
      nodes
    }
  };

  markPending(action['add-nodes'].nodes);
  action['add-nodes'].resource.ship = action['add-nodes'].resource.ship.slice(1);
  // this.store.handleEvent({ data: { 'graph-update': action } });// TODO address this.store
  return hookAction(action);
}

export const removeNodes = (
  ship: Patp,
  name: string,
  indices: string[]
): Poke<any> => {
  return hookAction({
    'remove-nodes': {
      resource: { ship, name },
      indices
    }
  });
}

// TODO these abominations
// getKeys() {
//   return this.scry<any>('graph-store', '/keys')
//     .then((keys) => {
//       this.store.handleEvent({
//         data: keys
//       });
//     });
// }

// getTags() {
//   return this.scry<any>('graph-store', '/tags')
//     .then((tags) => {
//       this.store.handleEvent({
//         data: tags
//       });
//     });
// }

// getTagQueries() {
//   return this.scry<any>('graph-store', '/tag-queries')
//     .then((tagQueries) => {
//       this.store.handleEvent({
//         data: tagQueries
//       });
//     });
// }

// getGraph(ship: string, resource: string) {
//   return this.scry<any>('graph-store', `/graph/${ship}/${resource}`)
//     .then((graph) => {
//       this.store.handleEvent({
//         data: graph
//       });
//     });
// }

// async getNewest(ship: string, resource: string, count: number, index = '') {
//   const data = await this.scry<any>('graph-store', `/newest/${ship}/${resource}/${count}${index}`);
//   this.store.handleEvent({ data });
// }

// async getOlderSiblings(ship: string, resource: string, count: number, index = '') {
//   const idx = index.split('/').map(decToUd).join('/');
//   const data = await this.scry<any>('graph-store',
//      `/node-siblings/older/${ship}/${resource}/${count}${idx}`
//    );
//   this.store.handleEvent({ data });
// }

// async getYoungerSiblings(ship: string, resource: string, count: number, index = '') {
//   const idx = index.split('/').map(decToUd).join('/');
//   const data = await this.scry<any>('graph-store',
//      `/node-siblings/younger/${ship}/${resource}/${count}${idx}`
//    );
//   this.store.handleEvent({ data });
// }


// getGraphSubset(ship: string, resource: string, start: string, end: string) {
//   return this.scry<any>(
//     'graph-store',
//     `/graph-subset/${ship}/${resource}/${end}/${start}`
//   ).then((subset) => {
//     this.store.handleEvent({
//       data: subset
//     });
//   });
// }

// getNode(ship: string, resource: string, index: string) {
//   const idx = index.split('/').map(numToUd).join('/');
//   return this.scry<any>(
//     'graph-store',
//     `/node/${ship}/${resource}${idx}`
//   ).then((node) => {
//     this.store.handleEvent({
//       data: node
//     });
//   });
// }
