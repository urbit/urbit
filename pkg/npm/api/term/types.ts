//  outputs
//

export type TermUpdate =
  | Blit;

export type Tint =
  | null
  | 'r' | 'g' | 'b' | 'c' | 'm' | 'y' | 'k' | 'w'
  | { r: number, g: number, b: number };

export type Deco = null | 'br' | 'un' | 'bl';

export type Stye = {
  deco: Deco[],
  back: Tint,
  fore: Tint
};

export type Stub = {
  stye: Stye,
  text: string[]
}

export type Blit =
  | { bel: null }                                       //  make a noise
  | { clr: null }                                       //  clear the screen
  | { hop: number | { r: number, c: number } }          //  set cursor col/pos
  | { klr: Stub[] }                                     //  put styled
  | { put: string[] }                                   //  put text at cursor
  | { nel: null }                                       //  newline
  | { sag: { path: string, file: string } }             //  save to jamfile
  | { sav: { path: string, file: string } }             //  save to file
  | { url: string }                                     //  activate url
  | { wyp: null }                                       //  wipe cursor line

//  inputs
//

export type Bolt =
  | string
  | { aro: 'd' | 'l' | 'r' | 'u' }
  | { bac: null }
  | { del: null }
  | { hit: { r: number, c: number } }
  | { ret: null }

export type Belt =
  | Bolt
  | { mod: { mod: 'ctl' | 'met' | 'hyp', key: Bolt } }
  | { txt: Array<string> };

export type Task =
  | { belt: Belt }
  | { blew: { w: number, h: number } }
  | { flow: { term: string, apps: Array<{ who: string, app: string }> } }
  | { hail: null }
  | { hook: null }

export type SessionTask = { session: string } & Task
