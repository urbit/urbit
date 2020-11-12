/**
 * Martian embassy
 */

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
export type AppName = 'chat' | 'link' | 'contacts' | 'publish' | 'graph';

export function getTagFromFrond<O>(frond: O): keyof O {
  const tags = Object.keys(frond) as Array<keyof O>;
  const tag = tags[0];
  if(!tag) {
    throw new Error("bad frond");
  }
  return tag;
}

export type ShipRank = 'czar' | 'king' | 'duke' | 'earl' | 'pawn';


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
    S;
