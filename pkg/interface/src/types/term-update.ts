
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
