import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import create from 'zustand';
import produce from 'immer';


type Session  = { term: Terminal, fit: FitAddon };
type Sessions = { [id: string]: Session; }

export interface TermState {
  sessions: Sessions,
  selected: string,
  slogstream: null | EventSource,
  theme: 'auto' | 'light' | 'dark'
};

const useTermState = create<TermState>((set, get) => ({
  sessions: {} as Sessions,
  selected: '',  //  empty string is default session
  slogstream: null,
  theme: 'auto',
  set: (f: (draft: TermState) => void) => {
    set(produce(f));
  }
} as TermState))

export default useTermState;
