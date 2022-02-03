/**
 * Martian embassy
 */
import { BigIntOrderedMap } from "./BigIntOrderedMap";
export declare type Path = string;
export declare type Patp = string;
export declare type PatpNoSig = string;
export declare type Serial = string;
export declare type Jug<K, V> = Map<K, Set<V>>;
export declare type AppName = 'chat' | 'link' | 'contacts' | 'publish' | 'graph' | 'groups';
export declare type ShipRank = 'czar' | 'king' | 'duke' | 'earl' | 'pawn';
export declare type Action = 'poke' | 'subscribe' | 'ack' | 'unsubscribe' | 'delete';
export declare type SetElement<S> = S extends Set<(infer T)> ? T : never;
export declare type MapKey<M> = M extends Map<(infer K), any> ? K : never;
export declare type MapValue<M> = M extends Map<any, (infer V)> ? V : never;
/**
 * Turns sets into arrays and maps into objects so we can send them over the wire
 */
export declare type Enc<S> = S extends Set<any> ? Enc<SetElement<S>>[] : S extends Map<string, any> ? {
    [s: string]: Enc<MapValue<S>>;
} : S extends object ? {
    [K in keyof S]: Enc<S[K]>;
} : S extends BigIntOrderedMap<infer T> ? {
    [index: string]: T;
} : S;
export declare type Mark = string;
export interface Poke<Action> {
    ship?: string;
    app: string;
    mark: Mark;
    json: Action;
}
export interface Scry {
    app: string;
    path: string;
}
export interface Thread<Action> {
    inputMark: string;
    outputMark: string;
    threadName: string;
    body: Action;
}
