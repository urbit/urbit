import { GroupPolicy, makeResource, Resource, resourceFromPath } from '../groups';

import { decToUd, deSig, unixToDa, Scry } from '../lib';
import { Enc, Path, Patp, PatpNoSig, Poke, Thread } from '../lib/types';
import { Content, GraphChildrenPoke, GraphNode, GraphNodePoke, Post } from './types';
import { patp2dec } from 'urbit-ob';

export const GRAPH_UPDATE_VERSION = 2;

export const createBlankNodeWithChildPost = (
  ship: PatpNoSig,
  parentIndex = '',
  childIndex = '',
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

export const markPending = (nodes: any): any => {
  Object.keys(nodes).forEach((key) => {
    nodes[key].post.author = deSig(nodes[key].post.author);
    nodes[key].post.pending = true;
    if (nodes[key].children) {
      nodes[key].children = markPending(nodes[key].children);
    }
  });
  return nodes;
};

export const createPost = (
  ship: PatpNoSig,
  contents: Content[],
  parentIndex = '',
  childIndex = 'DATE_PLACEHOLDER'
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

const storeAction = <T>(data: T, version: number = GRAPH_UPDATE_VERSION): Poke<T> => ({
  app: 'graph-store',
  mark: `graph-update-${version}`,
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

const hookAction = <T>(data: T, version: number = GRAPH_UPDATE_VERSION): Poke<T> => ({
  app: 'graph-push-hook',
  mark: `graph-update-${version}`,
  json: data
});

const dmAction = <T>(data: T): Poke<T> => ({
  app: 'dm-hook',
  mark: 'dm-hook-action',
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
};

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
    ship
  }
});

export const deleteGraph = (
  ship: PatpNoSig,
  name: string
): Thread<any> => viewAction('graph-delete', {
  'delete': {
    resource: makeResource(`~${ship}`, name)
  }
});

export const leaveGraph = (
  ship: Patp,
  name: string
): Thread<any> => viewAction('graph-leave', {
  'leave': {
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
};

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
};

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
};

export const addNodes = (
  ship: Patp,
  name: string,
  nodes: Object
): Thread<any> => ({
  inputMark: `graph-update-${GRAPH_UPDATE_VERSION}`,
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
  const nodes: Record<string, GraphNode> = {};
  nodes[post.index] = {
    post,
    children: null
  };
  return addNodes(ship, name, nodes);
};

export const addNode = (
  ship: Patp,
  name: string,
  node: GraphNodePoke
): Thread<any> => {
  const nodes: Record<string, GraphNodePoke> = {};
  nodes[node.post.index] = node;

  return addNodes(ship, name, nodes);
};

export const createGroupFeed = (
  group: Resource,
  vip: any = ''
): Thread<any> => ({
  inputMark: 'graph-view-action',
  outputMark: 'resource',
  threadName: 'graph-create-group-feed',
  body: {
    'create-group-feed': {
      resource: group,
      vip
    }
  }
});

export const disableGroupFeed = (
  group: Resource
): Thread<any> => ({
  inputMark: 'graph-view-action',
  outputMark: 'json',
  threadName: 'graph-disable-group-feed',
  body: {
    'disable-group-feed': {
      resource: group
    }
  }
});

/**
 * Set dm-hook to screen new DMs or not
 *
 */
export const setScreen = (screen: boolean): Poke<any> => dmAction({ screen });

/**
 * Accept a pending DM request
 *
 * @param ship the ship to accept
 */
export const acceptDm = (ship: string) => dmAction({
  accept: ship
});

/**
 * Decline a pending DM request
 *
 * @param ship the ship to accept
 */
export const declineDm = (ship: string) => dmAction({
  decline: ship
});

/**
 * Remove posts from a set of indices
 *
 */
export const removePosts = (
  ship: Patp,
  name: string,
  indices: string[]
): Poke<any> => hookAction({
  'remove-posts': {
    resource: { ship, name },
    indices
  }
});

/**
 * Remove a DM message from our inbox
 *
 * @remarks
 * This does not remove the message from the recipients inbox
 */
export const removeDmMessage = (
  our: Patp,
  index: string
): Poke<any> => ({
  app: 'graph-store',
  mark: `graph-update-${GRAPH_UPDATE_VERSION}`,
  json: {
    'remove-posts': {
      resource: { ship: our, name: 'dm-inbox' },
      indices: [index]
    }
  }
});

/**
 * Send a DM to a ship
 *
 * @param our sender
 * @param ship recipient
 * @param contents contents of message
 */
export const addDmMessage = (our: PatpNoSig, ship: Patp, contents: Content[]): Poke<any> => {
  const post = createPost(our, contents, `/${patp2dec(ship)}`);
  const node: GraphNode = {
    post,
    children: null
  };
  return {
    app: 'dm-hook',
    mark: `graph-update-${GRAPH_UPDATE_VERSION}`,
    json: {
      'add-nodes': {
        resource: { ship: `~${our}`, name: 'dm-inbox' },
        nodes: {
          [post.index]: node
        }
      }
    }
  };
};

const encodeIndex = (idx: string) => idx.split('/').map(decToUd).join('/');

/**
 * Fetch newest (larger keys) nodes in a graph under some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export const getNewest = (
  ship: string,
  name: string,
  count: number,
  index = ''
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/siblings` +
        `/newest/lone/${count}${encodeIndex(index)}`
});

/**
 * Fetch nodes in a graph that are older (key is smaller) and direct
 * siblings of some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export const getOlderSiblings = (
  ship: string,
  name: string,
  count: number,
  index: string
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/siblings/older/lone/${count}${encodeIndex(index)}`
});

/**
 * Fetch nodes in a graph that are younger (key is larger) and direct
 * siblings of some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export const getYoungerSiblings = (
  ship: string,
  name: string,
  count: number,
  index: string
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/siblings/newer/lone/${count}${encodeIndex(index)}`
});

/**
 * Fetch all nodes in a graph under some index, without loading children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 */
export const getShallowChildren = (ship: string, name: string, index = '') => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/children/lone/~/~${encodeIndex(index)}`
});

/**
 * Fetch newest nodes in a graph as a flat map, including children,
 * optionally starting at a specified key
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param start key to start at
 *
 */
export const getDeepOlderThan = (
  ship: string,
  name: string,
  count: number,
  start = ''
) => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/siblings` +
        `/${start.length > 0 ? 'older' : 'oldest'}` +
        `/kith/${count}${encodeIndex(start)}`
});

/**
 * Fetch a flat map of a nodes ancestors and firstborn children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 *
 */
export const getFirstborn = (
  ship: string,
  name: string,
  index: string
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/firstborn${encodeIndex(index)}`
});

/**
 * Fetch a single node, and all it's children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 *
 */
export const getNode = (
  ship: string,
  name: string,
  index: string
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}/node/index/kith${encodeIndex(index)}`
});

/**
 * Fetch entire graph
 *
 * @param ship ship of graph
 * @param name name of graph
 *
 */
export const getGraph = (
  ship: string,
  name: string
): Scry => ({
  app: 'graph-store',
  path: `/graph/${ship}/${name}`
});
