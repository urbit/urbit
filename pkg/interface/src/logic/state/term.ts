import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';

import { createState } from "./base";

type Session  = { term: Terminal, fit: FitAddon };
type Sessions = { [id: string]: Session; }

export interface TermState {
  sessions: Sessions,
  selected: string,
  slogstream: null | EventSource,
};

const useTermState = createState<TermState>('Term', {
  sessions: {},
  selected: '',  //  empty string is default session
  slogstream: null,
}, ['sessions', 'slogstream']);  //TODO  consider persisting

export default useTermState;