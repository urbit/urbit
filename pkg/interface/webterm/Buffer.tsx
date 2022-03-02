import { Terminal, ITerminalOptions } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import bel from './lib/bel';
import api from './api';

import {
  Belt, pokeTask, pokeBelt
} from '@urbit/api/term';
import { Session } from './state';
import { useCallback, useEffect, useRef } from 'react';
import useTermState from './state';
import React from 'react';
import { Box, Col } from '@tlon/indigo-react';
import { makeTheme } from './lib/theme';
import { useDark } from './join';
import { showBlit, csi, showSlog } from './lib/blit';

const termConfig: ITerminalOptions = {
  logLevel: 'warn',
  //
  convertEol: true,
  //
  rows: 24,
  cols: 80,
  scrollback: 10000,
  //
  fontFamily: '"Source Code Pro", "Roboto mono", "Courier New", monospace',
  fontWeight: 400,
  // NOTE  theme colors configured dynamically
  //
  bellStyle: 'sound',
  bellSound: bel,
  //
  //  allows text selection by holding modifier (option, or shift)
  macOptionClickForcesSelection: true
};

const readInput = (term: Terminal, e: string): Belt[] => {
  const belts: Belt[] = [];
  let strap = '';

  while (e.length > 0) {
    let c = e.charCodeAt(0);

    //  text input
    //
    if (c >= 32 && c !== 127) {
      strap += e[0];
      e = e.slice(1);
      continue;
    } else if ('' !== strap) {
      belts.push({ txt: strap.split('') });
      strap = '';
    }

    //  special keys/characters
    //
    if (0 === c) {
      term.write('\x07');  //  bel
    } else if (8 === c || 127 === c) {
      belts.push({ bac: null });
    } else if (13 === c) {
      belts.push({ ret: null });
    } else if (c <= 26) {
      let k = String.fromCharCode(96 + c);
      //NOTE  prevent remote shut-downs
      if ('d' !== k) {
        belts.push({ mod: { mod: 'ctl', key: k } });
      }
    }

    //  escape sequences
    //
    if (27 === c) {  //  ESC
      e = e.slice(1);
      c = e.charCodeAt(0);
      if (91 === c || 79 === c) {  //  [ or O
        e = e.slice(1);
        c = e.charCodeAt(0);
        /* eslint-disable max-statements-per-line */
        switch (c) {
          case 65: belts.push({ aro: 'u' }); break;
          case 66: belts.push({ aro: 'd' }); break;
          case 67: belts.push({ aro: 'r' }); break;
          case 68: belts.push({ aro: 'l' }); break;
        //
          case 77: {
            const m = e.charCodeAt(1) - 31;
            if (1 === m) {
              const c = e.charCodeAt(2) - 32;
              const r = e.charCodeAt(3) - 32;
              belts.push({ hit: { r: term.rows - r, c: c - 1 } });
            }
            e = e.slice(3);
            break;
          }
        //
          default: term.write('\x07'); break;  //  bel
        }
      } else if (c >= 97 && c <= 122) {  //  a <= c <= z
        belts.push({ mod: { mod: 'met', key: e[0] } });
      } else if (c === 46) {  //  .
        belts.push({ mod: { mod: 'met', key: '.' } });
      } else if (c === 8 || c === 127) {
        belts.push({ mod: { mod: 'met', key: { bac: null } } });
      } else {
        term.write('\x07'); break;  //  bel
      }
    }

    e = e.slice(1);
  }
  if ('' !== strap) {
    belts.push({ txt: strap.split('') });
    strap = '';
  }
  return belts;
};

const onResize = (session: Session) => () => {
  //TODO  debounce, if it ever becomes a problem
  //TODO  test that we only send this to the selected session,
  //      and that we *do* send it on-selected-change if necessary.
  session?.fit.fit();
};

const onInput = (name: string, session: Session, e: string) => {
  if (!session) {
    return;
  }
  const term = session.term;
  const belts = readInput(term, e);
  belts.map((b) => {
    api.poke(pokeBelt(name, b));
  });
};

interface BufferProps {
  name: string,
  selected: boolean,
}

export default function Buffer({ name, selected }: BufferProps) {
  const container = useRef<HTMLDivElement>(null);
  const dark = useDark();

  const session: Session = useTermState(s => s.sessions[name]);

  const initSession = useCallback(async (name: string, dark: boolean) => {
    console.log('setting up', name);

    //  set up xterm terminal
    //
    const term = new Terminal(termConfig);
    term.setOption('theme', makeTheme(dark));
    const fit = new FitAddon();
    term.loadAddon(fit);
    fit.fit();
    term.focus();

    //  start mouse reporting
    //
    term.write(csi('?9h'));

    const ses: Session = { term, fit, hasBell: false };

    //  set up event handlers
    //
    term.onData(e => onInput(name, ses, e));
    term.onBinary(e => onInput(name, ses, e));
    term.onResize((e) => {
      api.poke(pokeTask(name, { blew: { w: e.cols, h: e.rows } }));
    });

    //  open subscription
    //
    await api.subscribe({ app: 'herm', path: '/session/'+name+'/view',
      event: (e) => {
        showBlit(ses.term, e);
        if (e.bel && !selected) {
          useTermState.getState().set(state => {
            state.sessions[name].hasBell = true;
          });
        }
        //TODO  should handle %bye on this higher level though, for deletion
      },
      quit: () => {  //  quit
        // TODO  show user a message
        console.error('oops quit, pls handle');
      }
    });

    useTermState.getState().set((state) => {
      state.sessions[name] = ses;
    });
  }, []);

  // init session
  useEffect(() => {
    if(session) {
      return;
    }

    initSession(name, dark);
  }, [name]);

  //  on selected change, maybe setup the term, or put it into the container
  //
  const setContainer = useCallback((containerRef: HTMLDivElement | null) => {
    let newContainer = containerRef || container.current;
    if(session && newContainer) {
      container.current = newContainer;
      console.log('newcont', newContainer);
      // session.term.open(newContainer);
    } else {
      console.log('kaboom', session);
    }
  }, [session]);

    //  on-init, open slogstream and fetch existing sessions
  //
  useEffect(() => {

    window.addEventListener('resize', onResize(session));

    return () => {
      // TODO  clean up subs?
      window.removeEventListener('resize', onResize(session));
    };
  }, []);

  //  on dark mode change, change terminals' theme
  //
  useEffect(() => {
    const theme = makeTheme(dark);
    if (session) {
      session.term.setOption('theme', theme);
    }
    if (container.current) {
      container.current.style.backgroundColor = theme.background || '';
    }
  }, [dark]);

  useEffect(() => {
    if (session && selected && !session.term.isOpen) {
      session!.term.open(container.current);
      session!.fit.fit();
      session!.term.focus();
      session!.term.isOpen = true;
    }
  }, [selected, session]);

  return (
    !session && !selected ?
      <p>Loading...</p>
    :
      <Box
        width='100%'
        height='100%'
        bg='white'
        fontFamily='mono'
        overflow='hidden'
        style={selected ? {} : {display: 'none'}}
      >
      <Col
        width='100%'
        height='100%'
        minHeight='0'
        px={['0','2']}
        pb={['0','2']}
        ref={setContainer}
      >
      </Col>
    </Box>
  );
}
