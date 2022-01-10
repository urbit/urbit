/* eslint-disable max-lines */
import React, {
  useEffect,
  useRef,
  useCallback
} from 'react';

import useTermState from './state';
import { useDark } from './join';
import api from './api';

import { Terminal, ITerminalOptions, ITheme } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { saveAs } from 'file-saver';

import { Box, Col, Reset, _dark, _light } from '@tlon/indigo-react';

import 'xterm/css/xterm.css';

import {
  Belt, Blit, Stye, Stub, Tint, Deco,
  pokeTask, pokeBelt
} from '@urbit/api/term';

import bel from './lib/bel';
import { ThemeProvider } from 'styled-components';

type TermAppProps = {
  ship: string;
}

const makeTheme = (dark: boolean): ITheme => {
  let fg, bg: string;
  if (dark) {
    fg = 'white';
    bg = 'rgb(26,26,26)';
  } else {
    fg = 'black';
    bg = 'white';
  }
  // TODO  indigo colors.
  //      we can't pluck these from ThemeContext because they have transparency.
  //      technically xterm supports transparency, but it degrades performance.
  return {
    foreground: fg,
    background: bg,
    brightBlack: '#7f7f7f',  // NOTE  slogs
    cursor: fg,
    cursorAccent: bg,
    selection: fg
  };
};

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

const csi = (cmd: string, ...args: number[]) => {
  return '\x1b[' + args.join(';') + cmd;
};

const tint = (t: Tint) => {
  switch (t) {
    case null: return '9';
    case 'k':  return '0';
    case 'r':  return '1';
    case 'g':  return '2';
    case 'y':  return '3';
    case 'b':  return '4';
    case 'm':  return '5';
    case 'c':  return '6';
    case 'w':  return '7';
    default:   return `8;2;${t.r%256};${t.g%256};${t.b%256}`;
  }
};

const stye = (s: Stye) => {
  let out = '';

  //  text decorations
  //
  if (s.deco.length > 0) {
    out += s.deco.reduce((decs: number[], deco: Deco) => {
      /* eslint-disable max-statements-per-line */
      switch (deco) {
        case null: decs.push(0); return decs;
        case 'br': decs.push(1); return decs;
        case 'un': decs.push(4); return decs;
        case 'bl': decs.push(5); return decs;
        default: console.log('weird deco', deco); return decs;
      }
    }, []).join(';');
  }

  //  background color
  //
  if (s.back !== null) {
    if (out !== '') {
      out += ';';
    }
    out += '4';
    out += tint(s.back);
  }

  //  foreground color
  //
  if (s.fore !== null) {
    if (out !== '') {
      out += ';';
    }
    out += '3';
    out += tint(s.fore);
  }

  if (out === '') {
    return out;
  }
  return '\x1b[' + out + 'm';
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
    //TODO  remove for new backend
    {
      out += csi('H', term.rows, 1);
      out += csi('K');
    }
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
  //TODO  remove for new backend
  } else if ('lin' in blit) {
    out += csi('H', term.rows, 1);
    out += csi('K');
    out += blit.lin.join('');
  } else if ('mor' in blit) {
    out += '\n';
  } else {
    console.log('weird blit', blit);
  }

  term.write(out);
};

// NOTE  should generally only be passed the default terminal session
const showSlog = (term: Terminal, slog: string) => {
  //  set scroll region to exclude the bottom line,
  //  scroll up one line,
  //  move cursor to start of the newly created whitespace,
  //  set text to grey,
  //  print the slog,
  //  restore color, scroll region, and cursor.
  //
  term.write(csi('r', 1, term.rows - 1)
           + csi('S', 1)
           + csi('H', term.rows - 1, 1)
           + csi('m', 90)
           + slog
           + csi('m', 0)
           + csi('r')
           + csi('u'));
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
        belts.push({ ctl: k });
        //TODO  for new backend
        // belts.push({ mod: { mod: 'ctl', key: k } });
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
              //TODO  re-enable for new backend
              // belts.push({ hit: { r: term.rows - r, c: c - 1 } });
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

export default function TermApp(props: TermAppProps) {
  const container = useRef<HTMLDivElement>(null);
  // TODO  allow switching of selected
  const { sessions, selected, slogstream, set } = useTermState();
  const session = sessions[selected];
  const dark = useDark();

  const setupSlog = useCallback(() => {
    console.log('slog: setting up...');
    let available = false;
    const slog = new EventSource('/~_~/slog', { withCredentials: true });

    slog.onopen = (e) => {
      console.log('slog: opened stream');
      available = true;
    };

    slog.onmessage = (e) => {
      const session = useTermState.getState().sessions[''];
      if (!session) {
        console.log('default session mia!', 'slog:', slog);
        return;
      }
      showSlog(session.term, e.data);
    };

    slog.onerror = (e) => {
      console.error('slog: eventsource error:', e);
      if (available) {
        window.setTimeout(() => {
          if (slog.readyState !== EventSource.CLOSED) {
            return;
          }
          console.log('slog: reconnecting...');
          setupSlog();
        }, 10000);
      }
    };

    set((state) => {
      state.slogstream = slog;
    });
  }, [sessions]);

  const onInput = useCallback((ses: string, e: string) => {
    const term = useTermState.getState().sessions[ses].term;
    const belts = readInput(term, e);
    belts.map((b) => {  // NOTE  passing api.poke(pokeBelt makes `this` undefined!
      //TODO  pokeBelt(ses, b);
      api.poke({
        app: 'herm',
        mark: 'belt',
        json: b
      });
    });
  }, [sessions]);

  const onResize = useCallback(() => {
    // TODO  debounce, if it ever becomes a problem
    session?.fit.fit();
  }, [session]);

  //  on-init, open slogstream
  //
  useEffect(() => {
    if (!slogstream) {
      setupSlog();
    }
    window.addEventListener('resize', onResize);
    return () => {
      // TODO  clean up subs?
      window.removeEventListener('resize', onResize);
    };
  }, [onResize, setupSlog]);

  //  on dark mode change, change terminals' theme
  //
  useEffect(() => {
    const theme = makeTheme(dark);
    for (const ses in sessions) {
      sessions[ses].term.setOption('theme', theme);
    }
    if (container.current) {
      container.current.style.backgroundColor = theme.background || '';
    }
  }, [dark, sessions]);

  //  on selected change, maybe setup the term, or put it into the container
  //
  useEffect(() => {
    let ses = session;
    //  initialize terminal
    //
    if (!ses) {
      //  set up terminal
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
      term.onData(e => onInput(selected, e));
      term.onBinary(e => onInput(selected, e));
      term.onResize((e) => {
        //TODO  re-enable once new backend lands
        // api.poke(pokeTask(selected, { blew: { w: e.cols, h: e.rows } }));
      });

      ses = { term, fit };

      //  open subscription
      //
      api.subscribe({ app: 'herm', path: '/session/'+selected+'/view',
        event: (e) => {
          const ses = useTermState.getState().sessions[selected];
          if (!ses) {
            console.log('on blit: no such session', selected, sessions, useTermState.getState().sessions);
            return;
          }
          showBlit(ses.term, e);
        },
        quit: () => {  //  quit
          // TODO  show user a message
        }
      });
    }

    if (container.current && !container.current.contains(ses.term.element || null)) {
      ses.term.open(container.current);
      ses.fit.fit();
      ses.term.focus();
    }

    set((state) => {
      state.sessions[selected] = ses;
    });

    return () => {
      // TODO  unload term from container
      //      but term.dispose is too powerful? maybe just empty the container?
    };
  }, [set, session, container]);

  return (
    <>
      <ThemeProvider theme={dark ? _dark : _light}>
        <Reset />
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
      </ThemeProvider>
    </>
  );
}

// function harkContentsToPlainText(contents: HarkContent[]) {
//   return contents
//     .map((c) => {
//       if ('ship' in c) {
//         return c.ship;
//       }
//       return c.text;
//     })
//     .join('');
// }

// api.subscribe({
//   app: 'hark-store',
//   path: '/notes',
//   event: (u: any) => {
//     if ('add-note' in u) {
//       const { bin, body } = u['add-note'];
//       const binId = harkBinToId(bin);
//       const { title, content } = body;

//       ReactNativeWebView?.postMessage(...);

//       const note = new Notification(harkContentsToPlainText(title), {
//         body: harkContentsToPlainText(content),
//         tag: binId,
//         renotify: true
//       });
//       note.onclick = () => {};
//     }
//   }
// });
