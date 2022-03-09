import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import create from 'zustand';
import produce from 'immer';

export type Session = {
  term: Terminal,
  fit: FitAddon,
  hasBell: boolean,
  subscriptionId: number | null,
} | null;
export type Sessions = { [id: string]: Session; }

export interface TermState {
  sessions: Sessions,
  names: string[],
  selected: string,
  slogstream: null | EventSource,
  theme: 'auto' | 'light' | 'dark',
  //TODO: figure out the type
  set: any,
}

// eslint-disable-next-line no-unused-vars
const useTermState = create<TermState>((set, get) => ({
  sessions: {} as Sessions,
  names: [''],
  selected: '',  //  empty string is default session
  slogstream: null,
  theme: 'auto',
  // eslint-disable-next-line no-unused-vars
  set: (f: (draft: TermState) => void) => {
    set(produce(f));
  }
} as TermState));

export default useTermState;
