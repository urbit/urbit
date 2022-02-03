import { BigInteger } from 'big-integer';
import { Poke } from '../lib/types';
import { HarkBin, HarkBinId, HarkLid, HarkPlace } from './types';
export declare const harkAction: <T>(data: T) => Poke<T>;
declare const graphHookAction: <T>(data: T) => Poke<T>;
export { graphHookAction as harkGraphHookAction };
declare const groupHookAction: <T>(data: T) => Poke<T>;
export { groupHookAction as harkGroupHookAction };
export declare const actOnNotification: (frond: string, intTime: BigInteger, bin: HarkBin) => Poke<unknown>;
export declare const setMentions: (mentions: boolean) => Poke<unknown>;
export declare const setWatchOnSelf: (watchSelf: boolean) => Poke<unknown>;
export declare const setDoNotDisturb: (dnd: boolean) => Poke<unknown>;
export declare const archive: (bin: HarkBin, lid: HarkLid) => Poke<unknown>;
export declare const opened: Poke<{
    opened: any;
}>;
export declare const markCountAsRead: (place: HarkPlace) => Poke<unknown>;
export declare const markEachAsRead: (place: HarkPlace, path: string) => Poke<unknown>;
export declare const seen: () => Poke<{
    seen: any;
}>;
export declare const readAll: Poke<{
    'read-all': any;
}>;
export declare const archiveAll: Poke<{
    'archive-all': any;
}>;
export declare const ignoreGroup: (group: string) => Poke<unknown>;
export declare const ignoreGraph: (graph: string, index: string) => Poke<unknown>;
export declare const listenGroup: (group: string) => Poke<unknown>;
export declare const listenGraph: (graph: string, index: string) => Poke<unknown>;
/**
 * Read all graphs belonging to a particular group
 */
export declare const readGroup: (group: string) => Poke<{
    'read-group': string;
}>;
/**
 * Read all unreads in a graph
 */
export declare const readGraph: (graph: string) => Poke<{
    'read-graph': string;
}>;
export declare function harkBinToId(bin: HarkBin): HarkBinId;
export declare function harkBinEq(a: HarkBin, b: HarkBin): boolean;
export declare function harkLidToId(lid: HarkLid): string;
