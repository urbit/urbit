import { Terminal, ITerminalOptions } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { saveAs } from 'file-saver';
import bel from './lib/bel';
import api from './api';

import {
  Belt, Blit, Stub, pokeTask, pokeBelt
} from '@urbit/api/term';
import { Session } from './state';
import { useEffect, useRef } from 'react';
import useTermState from './state';
import React from 'react';
import { Box, Col } from '@tlon/indigo-react';
import { stye } from 'lib/stye';
import { makeTheme } from 'lib/theme';
import { useDark } from 'join';
import { useSlogstream } from 'lib/useSlogstream';
import { csi } from 'lib/csi';

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

const showBlit = (term: Terminal, blit: Blit) => {
  let out = '';

  if ('bel' in blit) {
    out += '\x07';
  } else if ('clr' in blit) {
    term.clear();
    out += csi('u');
  } else if ('hop' in blit) {
    if (typeof blit.hop === 'number') {
      out += csi('H', term.rows, blit.hop + 1);
    } else {
      out += csi('H', term.rows - blit.hop.r, blit.hop.c + 1);
    }
    out += csi('s');  //  save cursor position
  } else if ('put' in blit) {
    out += blit.put.join('');
    out += csi('u');
  } else if ('klr' in blit) {
    out += blit.klr.reduce((lin: string, p: Stub) => {
      lin += stye(p.stye);
      lin += p.text.join('');
      lin += csi('m', 0);
      return lin;
    }, '');
    out += csi('u');
  } else if ('nel' in blit) {
    out += '\n';
  } else if ('sag' in blit || 'sav' in blit) {
    const sav = ('sag' in blit) ? blit.sag : blit.sav;
    const name = sav.path.split('/').slice(-2).join('.');
    const buff = Buffer.from(sav.file, 'base64');
    const blob = new Blob([buff], { type: 'application/octet-stream' });
    saveAs(blob, name);
  } else if ('url' in blit) {
    window.open(blit.url);
  } else if ('wyp' in blit) {
    out += '\r' + csi('K');
    out += csi('u');
  //
  } else {
    console.log('weird blit', blit);
  }

  term.write(out);
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

const initSession = (name: string, dark: boolean) => {
  console.log('setting up', name);

  //  set up xterm terminal
  //
  const term = new Terminal(termConfig);
  term.setOption('theme', makeTheme(dark));
  const fit = new FitAddon();
  term.loadAddon(fit);

  //  start mouse reporting
  //
  term.write(csi('?9h'));

  //  set up event handlers
  //
  term.onData(e => onInput(name, e));
  term.onBinary(e => onInput(name, e));
  term.onResize((e) => {
    api.poke(pokeTask(name, { blew: { w: e.cols, h: e.rows } }));
  });

  const ses: Session = { term, fit, hasBell: false };

  //  open subscription
  //
  // api.poke(pokeTask('yy', { open: { term: 'hood', apps: [{ who: '~zod', app: 'dojo' }] } }));
  api.subscribe({ app: 'herm', path: '/session/'+name+'/view',
    event: (e) => {
      showBlit(ses!.term, e);
      if (e.bel) {
        ses.hasBell = true;
      }
      //TODO  should handle %bye on this higher level though, for deletion
    },
    quit: () => {  //  quit
      // TODO  show user a message
      console.error('oops quit, pls handle');
    }
  });

  return ses;
};

const onResize = (session: Session) => {
  //TODO  debounce, if it ever becomes a problem
  //TODO  maybe should send to other sessions on-selection-switch?
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
}

export default function Buffer({ name }: BufferProps) {
  const container = useRef<HTMLDivElement>(null);

  let session = useTermState(s => s.sessions[name]);

  useSlogstream(session);

  const dark = useDark();

  useEffect(() => {
    if (!session) {
      session = initSession(name, dark);
      useTermState.getState().set((state) => {
        state.sessions[name] = session;
      });
    }
  }, []);

    //  on-init, open slogstream and fetch existing sessions
  //
  useEffect(() => {

    window.addEventListener('resize', onResize);

    return () => {
      // TODO  clean up subs?
      window.removeEventListener('resize', onResize);
    };
  }, []);

  //  on dark mode change, change terminals' theme
  //
  useEffect(() => {
    // If the default session is not yet available
    if(!sessions['']) {
      return;
    }
    const theme = makeTheme(dark);
    for (const ses in sessions) {
      if (sessions[ses]) {
        sessions[ses].term.setOption('theme', theme);
      }
    }
    if (container.current) {
      container.current.style.backgroundColor = theme.background || '';
    }
  }, [dark, sessions]);

  //  on selected change, maybe setup the term, or put it into the container
  //
  useEffect(() => {
    console.log('session selection change', selected);

    let ses = session;

    if (container.current && !container.current.contains(ses.term.element || null)) {
      ses.term.open(container.current);
      ses.fit.fit();
      ses.term.focus();
    }

    set((state) => {
      state.sessions[selected] = ses;
    });

    return () => {
      //TODO  unload term from container
      //      but term.dispose is too powerful? maybe just empty the container?
    };
  }, [set, session, container]);

  return (
    !session ?
      <p>Loading...</p>
    :
      <Box
        width='100%'
        height='100%'
        bg='white'
        fontFamily='mono'
        overflow='hidden'
      >
      <Col
        width='100%'
        height='100%'
        minHeight='0'
        px={['0','2']}
        pb={['0','2']}
        ref={container}
      >
      </Col>
    </Box>
  );
}

