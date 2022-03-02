import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import create from 'zustand';
import produce from 'immer';

export type Session  = { term: Terminal, fit: FitAddon, hasBell: boolean } | null;
export type Sessions = { [id: string]: Session; }

export interface TermState {
  sessions: Sessions,
  selected: string,
  slogstream: null | EventSource,
  theme: 'auto' | 'light' | 'dark',
  //TODO: figure out the type
  set: any,
}

const useTermState = create<TermState>((set, get) => ({
  sessions: {} as Sessions,
  selected: '',  //  empty string is default session
  slogstream: null,
  theme: 'auto',
  set: (f: (draft: TermState) => void) => {
    set(produce(f));
  }
} as TermState));

export default useTermState;
