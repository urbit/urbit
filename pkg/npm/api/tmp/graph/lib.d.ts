import { GroupPolicy, Resource } from '../groups';
import { Scry } from '../lib';
import { Enc, Path, Patp, PatpNoSig, Poke, Thread } from '../lib/types';
import { Content, GraphNodePoke, Post } from './types';
export declare const GRAPH_UPDATE_VERSION = 3;
export declare const createBlankNodeWithChildPost: (ship: PatpNoSig, parentIndex: string, childIndex: string, contents: Content[]) => GraphNodePoke;
export declare const markPending: (nodes: any) => any;
export declare const createPost: (ship: PatpNoSig, contents: Content[], parentIndex?: string, childIndex?: string) => Post;
declare const storeAction: <T>(data: T, version?: number) => Poke<T>;
export { storeAction as graphStoreAction };
declare const viewAction: <T>(threadName: string, action: T) => Thread<T>;
export { viewAction as graphViewAction };
declare const hookAction: <T>(data: T, version?: number) => Poke<T>;
export { hookAction as graphHookAction };
export declare const createManagedGraph: (ship: PatpNoSig, name: string, title: string, description: string, group: Path, mod: string) => Thread<any>;
export declare const createUnmanagedGraph: (ship: PatpNoSig, name: string, title: string, description: string, policy: Enc<GroupPolicy>, mod: string) => Thread<any>;
export declare const joinGraph: (ship: Patp, name: string) => Thread<any>;
export declare const deleteGraph: (ship: PatpNoSig, name: string) => Thread<any>;
export declare const leaveGraph: (ship: Patp, name: string) => Thread<any>;
export declare const groupifyGraph: (ship: Patp, name: string, toPath?: string) => Thread<any>;
export declare const evalCord: (cord: string) => Thread<any>;
export declare const addGraph: (ship: Patp, name: string, graph: any, mark: any) => Poke<any>;
export declare const addNodes: (ship: Patp, name: string, nodes: Object) => Thread<any>;
export declare const addPost: (ship: Patp, name: string, post: Post) => Thread<any>;
export declare const addNode: (ship: Patp, name: string, node: GraphNodePoke) => Thread<any>;
export declare const createGroupFeed: (group: Resource, vip?: any) => Thread<any>;
export declare const disableGroupFeed: (group: Resource) => Thread<any>;
/**
 * Set dm-hook to screen new DMs or not
 *
 */
export declare const setScreen: (screen: boolean) => Poke<any>;
/**
 * Accept a pending DM request
 *
 * @param ship the ship to accept
 */
export declare const acceptDm: (ship: string) => Poke<{
    accept: string;
}>;
/**
 * Decline a pending DM request
 *
 * @param ship the ship to accept
 */
export declare const declineDm: (ship: string) => Poke<{
    decline: string;
}>;
/**
 * Remove posts from a set of indices
 *
 */
export declare const removePosts: (ship: Patp, name: string, indices: string[]) => Poke<any>;
/**
 * Remove a DM message from our inbox
 *
 * @remarks
 * This does not remove the message from the recipients inbox
 */
export declare const removeDmMessage: (our: Patp, index: string) => Poke<any>;
/**
 * Send a DM to a ship
 *
 * @param our sender
 * @param ship recipient
 * @param contents contents of message
 */
export declare const addDmMessage: (our: PatpNoSig, ship: Patp, contents: Content[]) => Poke<any>;
/**
 * Fetch newest (larger keys) nodes in a graph under some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export declare const getNewest: (ship: string, name: string, count: number, index?: string) => Scry;
/**
 * Fetch nodes in a graph that are older (key is smaller) and direct
 * siblings of some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export declare const getOlderSiblings: (ship: string, name: string, count: number, index: string) => Scry;
/**
 * Fetch nodes in a graph that are younger (key is larger) and direct
 * siblings of some index
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param count number of nodes to load
 * @param index index to query
 */
export declare const getYoungerSiblings: (ship: string, name: string, count: number, index: string) => Scry;
/**
 * Fetch all nodes in a graph under some index, without loading children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 */
export declare const getShallowChildren: (ship: string, name: string, index?: string) => {
    app: string;
    path: string;
};
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
export declare const getDeepOlderThan: (ship: string, name: string, count: number, start?: string) => {
    app: string;
    path: string;
};
/**
 * Fetch a flat map of a nodes ancestors and firstborn children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 *
 */
export declare const getFirstborn: (ship: string, name: string, index: string) => Scry;
/**
 * Fetch a single node, and all it's children
 *
 * @param ship ship of graph
 * @param name name of graph
 * @param index index to query
 *
 */
export declare const getNode: (ship: string, name: string, index: string) => Scry;
/**
 * Fetch entire graph
 *
 * @param ship ship of graph
 * @param name name of graph
 *
 */
export declare const getGraph: (ship: string, name: string) => Scry;
