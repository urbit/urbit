/**
 * Martian embassy
 */

import BigIntOrderedMap from "./BigIntOrderedMap";

// an urbit style path rendered as string
export type Path = string;

// patp including leading sig
export type Patp = string;

// patp excluding leading sig
export type PatpNoSig = string;

// @uvH encoded string
export type Serial = string;

// jug from hoon
export type Jug<K,V> = Map<K,Set<V>>;

// name of app
export type AppName = 'chat' | 'link' | 'contacts' | 'publish' | 'graph' | 'groups';

export type ShipRank = 'czar' | 'king' | 'duke' | 'earl' | 'pawn';

export type Action = 'poke' | 'subscribe' | 'ack' | 'unsubscribe' | 'delete';


export type SetElement<S> = S extends Set<(infer T)> ? T : never;
export type MapKey<M> = M extends Map<(infer K), any> ? K : never;
export type MapValue<M> = M extends Map<any, (infer V)> ? V : never;

/**
 * Turns sets into arrays and maps into objects so we can send them over the wire
 */
export type Enc<S> =
    S extends Set<any> ?
    Enc<SetElement<S>>[] :
    S extends Map<string, any> ?
    { [s: string]: Enc<MapValue<S>> } :
    S extends object ?
    { [K in keyof S]: Enc<S[K]> } :
    S extends BigIntOrderedMap<infer T> ?
    { [index: string]: T } :
    S;
    
export type Mark = string;

export interface Poke<Action> {
  ship?: string; // This should be handled by the http library, but is part of the spec
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
  desk: string;
}
