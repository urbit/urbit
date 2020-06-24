// an urbit style path render as string
export type Path = string;

// patp including leading sig
export type Patp = string;

// patp excluding leading sig
export type PatpNoSig = string;

// @uvH encoded string
export type Serial = string;

// name of app
export type AppName = 'chat' | 'link' | 'contacts' | 'publish';

export function getTagFromFrond<O>(frond: O): keyof O {
  const tags = Object.keys(frond) as Array<keyof O>;
  const tag = tags[0];
  if(!tag) {
    throw new Error("bad frond");
  }
  return tag;


}
