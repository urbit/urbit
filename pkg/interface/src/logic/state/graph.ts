import { Graphs, decToUd, numToUd, GraphNode, deSig, Association, resourceFromPath } from "@urbit/api";
import BigIntOrderedMap from "@urbit/api/lib/BigIntOrderedMap";
import {useCallback} from "react";
import { patp2dec } from 'urbit-ob';

import { BaseState, createState } from "./base";

export interface GraphState extends BaseState<GraphState> {
  graphs: Graphs;
  graphKeys: Set<string>;
  looseNodes: {
    [graph: string]: {
      [index: string]: GraphNode;
    }
  };
  pendingIndices: Record<string, any>;
  pendingDms: Set<string>;
  screening: boolean;
  graphTimesentMap: Record<string, any>;
  // getKeys: () => Promise<void>;
  // getTags: () => Promise<void>;
  // getTagQueries: () => Promise<void>;
  // getGraph: (ship: string, resource: string) => Promise<void>;
  // getNewest: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  // getOlderSiblings: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  // getYoungerSiblings: (ship: string, resource: string, count: number, index?: string) => Promise<void>;
  // getGraphSubset: (ship: string, resource: string, start: string, end: string) => Promise<void>;
  // getNode: (ship: string, resource: string, index: string) => Promise<void>;
};

const useGraphState = createState<GraphState>('Graph', {
  graphs: {},
  graphKeys: new Set(),
  looseNodes: {},
  pendingIndices: {},
  graphTimesentMap: {},
  pendingDms: new Set(),
  screening: false,
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
  // getNewest: async (
  //   ship: string,
  //   resource: string,
  //   count: number,
  //   index: string = ''
  // ) => {
  //   const api = useApi();
  //   const data = await api.scry({
  //     app: 'graph-store',
  //     path: `/newest/${ship}/${resource}/${count}${index}`
  //   });
  //   graphReducer(data);
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
}, ['graphs', 'graphKeys', 'looseNodes', 'graphTimesentMap']);

export function useGraph(ship: string, name: string) {
  return useGraphState(
    useCallback(s => s.graphs[`${deSig(ship)}/${name}`], [ship, name])
  );
}

export function useGraphForAssoc(association: Association) {
  const { resource } = association;
  const { ship, name } = resourceFromPath(resource);
  return useGraph(ship, name);
}

window.useGraphState = useGraphState;

export function useInbox() {
  return useGraphState(s => s.graphs[`${window.ship}/dm-inbox`] || new BigIntOrderedMap());
}

export function useDM(ship: string) {
  const inbox = useInbox();
  const shipGraph = inbox.get(patp2dec(ship));
  return shipGraph?.children ?? new BigIntOrderedMap();
}

export default useGraphState;
