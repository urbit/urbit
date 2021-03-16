import _ from 'lodash';
import { GroupPolicy, makeResource, resourceFromPath } from '../groups';

import { deSig, unixToDa } from '../lib';
import { Enc, Path, Patp, PatpNoSig, Poke, Thread } from '../lib/types';
import { Content, GraphChildrenPoke, GraphNode, GraphNodePoke, Post } from './types';

export const createBlankNodeWithChildPost = (
  ship: PatpNoSig,
  parentIndex: string = '',
  childIndex: string = '',
  contents: Content[]
): any => { // TODO should be GraphNode
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

export const markPending = (nodes: any): void => {
  _.forEach(nodes, node => {
    node.post.author = deSig(node.post.author);
    node.post.pending = true;
    markPending(node.children || {});
  });
};

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
): Thread<any> => viewAction('graph-create', {
  create: {
    resource: makeResource(`~${ship}`, name),
    title,
    description,
    associated: { policy },
    module: mod,
    mark: moduleToMark(mod)
  }
});

export const joinGraph = (
  ship: Patp,
  name: string
): Thread<any> => viewAction('graph-join', {
  join: {
    resource: makeResource(ship, name),
    ship,
  }
});

export const deleteGraph = (
  ship: PatpNoSig,
  name: string
): Thread<any> => viewAction('graph-delete', {
  "delete": {
    resource: makeResource(`~${ship}`, name)
  }
});

export const leaveGraph = (
  ship: Patp,
  name: string
): Thread<any> => viewAction('graph-leave', {
  "leave": {
    resource: makeResource(ship, name)
  }
});

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

export const addNodes = (
  ship: Patp,
  name: string,
  nodes: Object
): Thread<any> => ({
  inputMark: 'graph-update',
  outputMark: 'graph-view-action',
  threadName: 'graph-add-nodes',
  body: {
    'add-nodes': {
      resource: { ship, name },
      nodes
    }
  }
});

export const addPost = (
  ship: Patp,
  name: string,
  post: Post
): Thread<any> => {
  let nodes: Record<string, GraphNode> = {};
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
): Thread<any> => {
  let nodes: Record<string, GraphNode> = {};
  nodes[node.post.index] = node;

  return addNodes(ship, name, nodes);
}


export const removeNodes = (
  ship: Patp,
  name: string,
  indices: string[]
): Poke<any> => hookAction({
  'remove-nodes': {
    resource: { ship, name },
    indices
  }
});
