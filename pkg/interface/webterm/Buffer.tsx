import { Terminal, ITerminalOptions } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { debounce } from 'lodash';
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
import { showBlit, csi, hasBell } from './lib/blit';
import { DEFAULT_SESSION, RESIZE_DEBOUNCE_MS, RESIZE_THRESHOLD_PX } from './constants';
import { retry } from './lib/retry';

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
  macOptionClickForcesSelection: true,
  //  prevent insertion of simulated arrow keys on-altclick
  altClickMovesCursor: false
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
      e = e.slice(1);  //TODO  revisit wrt (list @c) & unicode characters
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
      const k = String.fromCharCode(96 + c);
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
              belts.push({ hit: { y: r - 1, x: c - 1 } });
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
    if (1 === strap.length) {
      belts.push(strap);
    } else {
      belts.push({ txt: strap.split('') });
    }
    strap = '';
  }
  return belts;
};

const onResize = async (name: string, session: Session) => {
  if (session) {
    session.fit.fit();
    useTermState.getState().set((state) => {
      state.sessions[name].pending++;
    });
    api.poke(pokeTask(name, { blew: { w: session.term.cols, h: session.term.rows } })).then(() => {
      useTermState.getState().set((state) => {
        state.sessions[name].pending--;
      });
    });
  }
};

const onInput = (name: string, session: Session, e: string) => {
  if (!session) {
    return;
  }
  const term = session.term;
  const belts = readInput(term, e);
  belts.forEach((b) => {
    useTermState.getState().set((state) => {
      state.sessions[name].pending++;
    });
    api.poke(pokeBelt(name, b)).then(() => {
      useTermState.getState().set((state) => {
        state.sessions[name].pending--;
      });
    });
  });
};

interface BufferProps {
  name: string,
  selected: boolean,
  dark: boolean,
}

export default function Buffer({ name, selected, dark }: BufferProps) {
  const containerRef = useRef<HTMLDivElement | null>(null);

  const session: Session = useTermState(s => s.sessions[name]);

  const initSession = useCallback(async (name: string, dark: boolean) => {
    console.log('setting up', name === DEFAULT_SESSION ? 'default' : name);

    //  set up xterm terminal
    //
    const term = new Terminal(termConfig);
    term.options.theme = makeTheme(dark);
    const fit = new FitAddon();
    term.loadAddon(fit);
    fit.fit();
    term.focus();

    //  start mouse reporting
    //
    term.write(csi('?9h'));

    const ses: Session = {
      term,
      fit,
      hasBell: false,
      pending: 0,
      subscriptionId: null
    };

    //  set up event handlers
    //
    term.attachCustomKeyEventHandler((e: KeyboardEvent) => {
      //NOTE  ctrl+shift keypresses never make it into term.onData somehow,
      //      so we handle them specially ourselves.
      //      we may be able to remove this once xterm.js fixes #3382 & co.
      if (e.shiftKey
       && e.ctrlKey
       && e.type === 'keydown'
       && e.key.length === 1
      ) {
        api.poke(pokeBelt(name, { mod: { mod: 'ctl', key: e.key } }));
        return false;
      }
      return true;
    });
    term.onData(e => onInput(name, ses, e));
    term.onBinary(e => onInput(name, ses, e));

    //  open subscription
    //
    const initSubscription = async () => {
      const subscriptionId = await api.subscribe({
        app: 'herm', path: '/session/' + name + '/view',
        event: (e) => {
          showBlit(ses.term, e);
          //NOTE  getting selected from state because selected prop is stale
          if (hasBell(e) && (useTermState.getState().selected !== name)) {
            useTermState.getState().set((state) => {
              state.sessions[name].hasBell = true;
            });
          }
          //TODO  should handle %bye on this higher level though, for deletion
        },
        err: (e, id) => {
          console.log(`subscription error, id ${id}:`, e);
        },
        quit: async () => {  //  quit
          console.error('quit, reconnecting...');
          try {
            const newSubscriptionId = await retry(initSubscription, () => {
              console.log('attempting to reconnect ...');
            }, 5);
            useTermState.getState().set((state) => {
              state.sessions[name].subscriptionId = newSubscriptionId;
            });
          } catch (error) {
            console.log('unable to reconnect', error);
          }
        }
      });

      return subscriptionId;
    };

    ses.subscriptionId = await initSubscription();

    useTermState.getState().set((state) => {
      state.sessions[name] = ses;
    });
  }, []);

  const shouldResize = useCallback(() => {
    if(!session) {
      return false;
    }

    const containerHeight = document.querySelector('.buffer-container')?.clientHeight || Infinity;
    const terminalHeight = session.term.element?.clientHeight || 0;

    return (containerHeight - terminalHeight) >= RESIZE_THRESHOLD_PX;
  }, [session]);

  const onSelect = useCallback(async () => {
    if (session && selected && shouldResize()) {
      session.fit.fit();
      await api.poke(pokeTask(name, { blew: { w: session.term.cols, h: session.term.rows } }));
      session.term.focus();
    }
  }, [session, selected]);

  // Effects
  // init session
  useEffect(() => {
    if(session) {
      return;
    }

    initSession(name, dark);
  }, [name]);

  // attach to DOM when ref is available
  useEffect(() => {
    if(session && containerRef.current && !session.term.element) {
      session.term.open(containerRef.current);
    }
  }, [session, containerRef]);

  //  initialize resize listeners
  //
  useEffect(() => {
    if(!session) {
      return;
    }

    // TODO: use ResizeObserver for improved performance?
    const debouncedResize =  debounce(() => onResize(name, session), RESIZE_DEBOUNCE_MS);
    window.addEventListener('resize', debouncedResize);

    return () => {
      window.removeEventListener('resize', debouncedResize);
    };
  }, [session]);

  //  on dark mode change, change terminals' theme
  //
  useEffect(() => {
    const theme = makeTheme(dark);
    if (session) {
      session.term.options.theme = theme;
    }
    if (containerRef.current) {
      containerRef.current.style.backgroundColor = theme.background || '';
    }
  }, [session, dark]);

  // On select, resize, focus, and poke herm with updated cols and rows
  useEffect(() => {
    onSelect();
  }, [onSelect]);

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
        className="terminal-container"
        style={selected ? { zIndex: 999 } : {}}
      >
        <Col
          width='100%'
          height='100%'
          minHeight='0'
          px={['0', '2']}
          pb={['0', '2']}
          ref={containerRef}
        >
        </Col>
      </Box>
  );
}
