import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import { patp2dec } from 'urbit-ob';
import shallow from 'zustand/shallow';

import { Association, deSig, GraphNode, Graphs, FlatGraphs, resourceFromPath, ThreadGraphs, getGraph, getShallowChildren, setScreen } from '@urbit/api';
import { useCallback } from 'react';
import { createState, createSubscription, reduceStateN, pokeOptimisticallyN } from './base';
import airlock from '~/logic/api';
import { addDmMessage, addPost, Content, getDeepOlderThan, getFirstborn, getNewest, getNode, getOlderSiblings, getYoungerSiblings, markPending, Post, addNode, GraphNodePoke } from '@urbit/api/graph';
import { GraphReducer, reduceDm } from '../reducers/graph-update';
import _ from 'lodash';
import { clone } from '../lib/util';

export interface GraphState {
  graphs: Graphs;
  graphKeys: Set<string>;
  looseNodes: {
    [graph: string]: {
      [index: string]: GraphNode;
    }
  };
  flatGraphs: FlatGraphs;
  threadGraphs: ThreadGraphs;
  pendingIndices: Record<string, any>;
  pendingDms: Set<string>;
  screening: boolean;
  graphTimesentMap: Record<number, string>;
  getShallowChildren: (ship: string, name: string, index?: string) => Promise<void>;
  getDeepOlderThan: (ship: string, name: string, count: number, start?: string) => Promise<void>;
  getNewest: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  getOlderSiblings: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  getYoungerSiblings: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  getNode: (ship: string, resource: string, index: string) => Promise<void>;
  getFirstborn: (ship: string, resource: string, index: string) => Promise<void>;
  getGraph: (ship: string, name: string) => Promise<void>;
  addDmMessage: (ship: string, contents: Content[]) => Promise<void>;
  addPost: (ship: string, name: string, post: Post) => Promise<void>;
  addNode: (ship: string, name: string, post: GraphNodePoke) => Promise<void>;
  setScreen: (screen: boolean) => void;
}
// @ts-ignore investigate zustand types
const useGraphState = createState<GraphState>('Graph', (set, get) => ({
  graphs: {},
  flatGraphs: {},
  threadGraphs: {},
  graphKeys: new Set(),
  looseNodes: {},
  pendingIndices: {},
  graphTimesentMap: {},
  pendingDms: new Set(),
  screening: false,
  addDmMessage: async (ship: string, contents: Content[]) => {
    const poke = addDmMessage(window.ship, ship, contents);
    const promise = airlock.poke(poke);
    const pending = clone(poke);
    markPending(pending.json['add-nodes'].nodes);
    pending.json['add-nodes'].resource.ship = pending.json['add-nodes'].resource.ship.slice(1);
    GraphReducer({
      'graph-update': pending.json
    });
    await promise;
  },
  addPost: async (ship, name, post) => {
    const thread = addPost(ship, name, post);
    const promise = airlock.thread(thread);
    const { body } = clone(thread);
    markPending(body['add-nodes'].nodes);
    body['add-nodes'].resource.ship = body['add-nodes'].resource.ship.slice(1);
    GraphReducer({
      'graph-update': body,
      'graph-update-flat': body,
      'graph-update-thread': body
    });
    await promise;
  },
  addNode: async (ship, name, node) => {
    const thread = addNode(ship, name, node);
    const promise = airlock.thread(thread);
    const { body } = clone(thread);
    markPending(body['add-nodes'].nodes);
    body['add-nodes'].resource.ship = body['add-nodes'].resource.ship.slice(1);
    GraphReducer({
      'graph-update': body,
      'graph-update-flat': body,
      'graph-update-thread': body
    });
    await promise;
  },
  getDeepOlderThan: async (ship, name, count, start) => {
    const data = await airlock.scry(getDeepOlderThan(ship, name, count, start));

    data['graph-update'].fetch = true;
    const node = data['graph-update'];
    GraphReducer({
      'graph-update': node,
      'graph-update-flat': node
    });
  },

  getFirstborn: async (ship, name,index) => {
    const data = await airlock.scry(getFirstborn(ship, name, index));
    data['graph-update'].fetch = true;
    const node = data['graph-update'];
    GraphReducer({
      'graph-update-thread': {
        index,
        ...node
      },
      'graph-update': node
    });
  },
  getNode: async (ship: string, name: string, index: string) => {
    const data = await airlock.scry(getNode(ship, name, index));
    data['graph-update'].fetch = true;
    const node = data['graph-update'];
    GraphReducer({
      'graph-update-loose': node
    });
  },
  getOlderSiblings: async (ship: string, name: string, count: number, index: string) => {
    const data = await airlock.scry(getOlderSiblings(ship, name, count, index));
    data['graph-update'].fetch = true;
    GraphReducer(data);
  },
  getYoungerSiblings: async (ship: string, name: string, count: number, index: string) => {
    const data = await airlock.scry(getYoungerSiblings(ship, name, count, index));
    data['graph-update'].fetch = true;
    GraphReducer(data);
  },
  getNewest: async (
    ship: string,
    name: string,
    count: number,
    index = ''
  ) => {
    const data = await airlock.scry(getNewest(ship, name, count, index));
    data['graph-update'].fetch = true;
    GraphReducer(data);
  },
  getGraph: async (ship, name) => {
    const data = await airlock.scry(getGraph(ship, name));
    GraphReducer(data);
  },
  getShallowChildren: async (ship: string, name: string, index = '') => {
    const data = await airlock.scry(getShallowChildren(ship, name, index));
    data['graph-update'].fetch = true;
    GraphReducer(data);
  },
  setScreen: (screen: boolean) => {
    const poke = setScreen(screen);
    pokeOptimisticallyN(useGraphState, poke, reduceDm);
  }
  // getKeys: async () => {
  //   const api = useApi();
  //   const keys = await api.scry({
  //     app: 'graph-store',
  //     path: '/keys'
  //   });
  //   graphReducer(keys);
  // },
  // getTags: async () => {
  //   const api = useApi();
  //   const tags = await api.scry({
  //     app: 'graph-store',
  //     path: '/tags'
  //   });
  //   graphReducer(tags);
  // },
  // getTagQueries: async () => {
  //   const api = useApi();
  //   const tagQueries = await api.scry({
  //     app: 'graph-store',
  //     path: '/tag-queries'
  //   });
  //   graphReducer(tagQueries);
  // },
  // getGraph: async (ship: string, resource: string) => {
  //   const api = useApi();
  //   const graph = await api.scry({
  //     app: 'graph-store',
  //     path: `/graph/${ship}/${resource}`
  //   });
  //   graphReducer(graph);
  // },
  // getOlderSiblings: async (
  //   ship: string,
  //   resource: string,
  //   count: number,
  //   index: string = ''
  // ) => {
  //   const api = useApi();
  //   index = index.split('/').map(decToUd).join('/');
  //   const data = await api.scry({
  //     app: 'graph-store',
  //     path: `/node-siblings/older/${ship}/${resource}/${count}${index}`
  //   });
  //   graphReducer(data);
  // },
  // getYoungerSiblings: async (
  //   ship: string,
  //   resource: string,
  //   count: number,
  //   index: string = ''
  // ) => {
  //   const api = useApi();
  //   index = index.split('/').map(decToUd).join('/');
  //   const data = await api.scry({
  //     app: 'graph-store',
  //     path: `/node-siblings/younger/${ship}/${resource}/${count}${index}`
  //   });
  //   graphReducer(data);
  // },
  // getGraphSubset: async (
  //   ship: string,
  //   resource: string,
  //   start: string,
  //   end: string
  // ) => {
  //   const api = useApi();
  //   const subset = await api.scry({
  //     app: 'graph-store',
  //     path: `/graph-subset/${ship}/${resource}/${end}/${start}`
  //   });
  //   graphReducer(subset);
  // },
  // getNode: async (
  //   ship: string,
  //   resource: string,
  //   index: string
  // ) => {
  //   const api = useApi();
  //   index = index.split('/').map(numToUd).join('/');
  //   const node = api.scry({
  //     app: 'graph-store',
  //     path: `/node/${ship}/${resource}${index}`
  //   });
  //   graphReducer(node);
  // },
}), [
  'graphs',
  'graphKeys',
  'looseNodes',
  'graphTimesentMap',
  'flatGraphs',
  'threadGraphs',
  'pendingDms'
], [
  (set, get) => createSubscription('graph-store', '/updates', (e) => {
    GraphReducer(e);
  }),
  (set, get) => createSubscription('graph-store', '/keys', (e) => {
    GraphReducer(e);
  }),

  (set, get) => createSubscription('dm-hook', '/updates', (e) => {
    const j = _.get(e, 'dm-hook-action', false);
    if(j) {
      reduceStateN(get(), j, reduceDm);
    }
  })],
  {
    graphs: {},
    looseNodes: {},
    graphTimesentMap: {},
    flatGraphs: {},
    threadGraphs: {},
    pendingDms: new Set<string>()
  }

);

export function useGraph(ship: string, name: string) {
  return useGraphState(
    useCallback(s => s.graphs[`${deSig(ship)}/${name}`], [ship, name])
  );
}

export function useFlatGraph(ship: string, name: string) {
  return useGraphState(
    useCallback(s => s.flatGraphs[`${deSig(ship)}/${name}`], [ship, name])
  );
}

export function useThreadGraph(ship: string, name: string, index: string) {
  return useGraphState(
    useCallback(s => s.threadGraphs[`${deSig(ship)}/${name}/${index}`], [
      ship,
      name,
      index
    ])
  );
}

export function useGraphTimesentMap(ship: string, name: string) {
  return useGraphState(
    useCallback(s => s.graphTimesentMap[`${deSig(ship)}/${name}`], [ship, name])
  );
}
const emptyObject = {};

export function useGraphTimesent(key: string) {
  return useGraphState(useCallback(s => s.graphTimesentMap[key] || emptyObject, [key]), shallow);
}
export function useGraphForAssoc(association: Association) {
  const { resource } = association;
  const { ship, name } = resourceFromPath(resource);
  return useGraph(ship, name);
}

export function useInbox() {
  return useGraphState(s => s.graphs[`${window.ship}/dm-inbox`] || new BigIntOrderedMap<GraphNode>());
}

export function useDM(ship: string) {
  const inbox = useInbox();
  const shipGraph = inbox.get(patp2dec(ship));
  return shipGraph?.children ?? new BigIntOrderedMap();
}

export default useGraphState;
