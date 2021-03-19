import React, {
  useEffect,
  useRef,
  useCallback
} from 'react';
import Helmet from 'react-helmet';

import useTermState from '~/logic/state/term';
import useSettingsState from "~/logic/state/settings";
import useLocalState from "~/logic/state/local";

import { Terminal, ITerminalOptions, ITheme } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { saveAs } from 'file-saver';

import { Box, Col } from '@tlon/indigo-react';

import '../../../../node_modules/xterm/css/xterm.css'
import GlobalApi from '~/logic/api/global';
import { Belt } from '~/logic/api/term';
import { Blit, Stye, Stub, Tint, Deco } from '~/types/term-update';

import bel from '~/logic/lib/bel';

type TermAppProps = {
  api: GlobalApi;
  ship: string;
  notificationsCount: number;
}

const makeTheme = (dark: boolean): ITheme => {
  let fg, bg: string;
  if (dark) {
    fg = 'white';
    bg = 'black';
  } else {
    fg = 'black';
    bg = 'white';
  }
  //TODO  indigo colors.
  //      we can't pluck these from ThemeContext because they have transparency.
  //      technically xterm supports transparency, but it degrades performance.
  return {
    foreground: fg,
    background: bg,
    brightBlack: '#7f7f7f',  //NOTE  slogs
    cursor: fg,
  }
}

const termConfig: ITerminalOptions = {
  logLevel: 'warn',
  //
  convertEol: true,
  //
  rows: 24,
  cols: 80,
  scrollback: 10000,
  //
  fontFamily: '"Source Code Pro","Roboto mono","Courier New",monospace',
  //NOTE  theme colors configured dynamically
  //
  bellStyle: 'sound',
  bellSound: bel,
  //
  //  allows text selection by holding modifier (option, or shift)
  macOptionClickForcesSelection: true,
}

const csi = (cmd: string, ...args: Array<number>) => {
  return '\x1b[' + args.join(';') + cmd;
}

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
}

const stye = (s: Stye) => {
  let out = '';

  //  text decorations
  //
  if (s.deco.length > 0) {
    out += s.deco.reduce((decs: Array<number>, deco: Deco) => {
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
    if (out !== '') out += ';';
    out += '4';
    out += tint(s.back);
  }

  //  foreground color
  //
  if (s.fore !== null) {
    if (out !== '') out += ';';
    out += '3';
    out += tint(s.fore);
  }

  if (out === '') return out;
  return '\x1b[' + out + 'm';
}

export default function TermApp(props: TermAppProps) {
  const { api } = props;

  const container = useRef<HTMLElement>(null);
  //TODO  allow switching of selected
  const { sessions, selected, set } = useTermState();

  const osDark = useLocalState((state) => state.dark);
  const theme = useSettingsState(s => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark);

  const onSlog = useCallback((slog) => {
    if (!sessions['']) {
      console.log('default session mia!', 'slog:', slog);
      return;
    }
    const term = sessions[''].term;

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
  }, [sessions['']]);

  //TODO  could be static function if we pass in Terminal explicitly?
  const onBlit = useCallback((ses: string, blit: Blit) => {
    //TODO
    if (!sessions[ses]) {
      console.log('on blit: no such session', ses);
      return;
    }

    const term = sessions[ses].term;
    let out = '';

    if ('bel' in blit) {
      out += '\x07';
    }
    else if ('clr' in blit) {
      term.clear();
      out += csi('u');
    }
    else if ('hop' in blit) {
      if (typeof blit.hop === 'number') {
        out += csi('H', term.rows, blit.hop + 1);
      }
      else {
        out += csi('H', term.rows - blit.hop.r, blit.hop.c + 1);
      }
      out += csi('s');  //  save cursor position
    }
    else if ('put' in blit) {
      out += blit.put.join('');
      out += csi('u');
    }
    else if ('klr' in blit) {
      out += blit.klr.reduce((lin: string, p: Stub) => {
        lin += stye(p.stye);
        lin += p.text.join('');
        lin += csi('m', 0);
        return lin;
      }, '');
      out += csi('u');
    }
    else if ('nel' in blit) {
      out += '\n';
    }
    else if ('sag' in blit || 'sav' in blit) {
      const sav = ('sag' in blit) ? blit.sag : blit.sav;
      let name = sav.path.split('/').slice(-2).join('.');
      let buff = new Buffer(sav.file, 'base64');
      let blob = new Blob([buff], {type: 'application/octet-stream'});
      saveAs(blob, name);
    }
    else if ('url' in blit) {
      window.open(blit.url);
    }
    else if ('wyp' in blit) {
      out += '\r' + csi('K');
      out += csi('u');
    }
    else {
      console.log('weird blit', blit);
    }

    term.write(out);
  }, [sessions]);

  const setupSlog = useCallback(() => {
    console.log('slog: setting up...');
    let available = false;
    const slog = new EventSource('/~_~/slog', { withCredentials: true });

    slog.onopen = e => {
      console.log('slog: opened stream');
      available = true;
    }

    slog.onmessage = e => {
      onSlog(e.data);
    }

    slog.onerror = e => {
      console.error('slog: eventsource error:', e);
      if (available) {
        window.setTimeout(() => {
          if (slog.readyState !== EventSource.CLOSED) return;
          console.log('slog: reconnecting...');
          setupSlog();
        }, 10000);
      }
    }
  }, [onSlog]);

  const onInput = useCallback((ses: string, e: string) => {
    const term = sessions[ses].term;
    let belts: Array<Belt> = [];
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
      if (0 === c) {
        term.write('\x07');  //  bel
      }
      else if (8 === c || 127 === c) {
        belts.push({ bac: null });
      }
      else if (13 === c) {
        belts.push({ ret: null });
      }
      else if (c <= 26) {
        belts.push({ mod: { mod: 'ctl', key: String.fromCharCode(96 + c) } });
      }

      //  escape sequences
      //
      if (27 === c) {  //  ESC
        e = e.slice(1);
        c = e.charCodeAt(0);
        if (91 === c || 79 === c) {  //  [ or O
          e = e.slice(1);
          c = e.charCodeAt(0);
          switch (c) {
            case 65: belts.push({ aro: 'u' }); break;
            case 66: belts.push({ aro: 'd' }); break;
            case 67: belts.push({ aro: 'r' }); break;
            case 68: belts.push({ aro: 'l' }); break;
          //
            case 77:
              const m = e.charCodeAt(1) - 31;
              if (1 === m) {
                const c = e.charCodeAt(2) - 32;
                const r = e.charCodeAt(3) - 32;
                belts.push({ hit: { r: term.rows - r, c: c - 1 } });
              }
              e = e.slice(3);
              break;
          //
            default: term.write('\x07'); break;  //  bel
          }
        }
        else if (c >= 97 && c <= 122) {  //  a <= c <= z
          belts.push({ mod: { mod: 'met', key: e[0] } });
        }
        else if (c === 46) {  //  .
          belts.push({ mod: { mod: 'met', key: '.' } });
        }
        else if (c === 8 || c === 127) {
          belts.push({ mod: { mod: 'met', key: { bac: null } } });
        }
        else {
          term.write('\x07'); break;  //  bel
        }
      }

      e = e.slice(1);
    }
    if ('' !== strap) {
      belts.push({ txt: strap.split('') });
      strap = '';
    }
    belts.map(b => {  //NOTE  passing api.term.sendBelt makes `this` undefined!
      api.term.sendBelt(b);
    });
  }, [sessions, api.term]);

  //  on-init, open slogstream
  //
  useEffect(() => {
    setupSlog();
    return () => {
      //TODO  clean up subs?
    };
  }, []);

  //  on dark mode change, change terminals' theme
  //
  useEffect(() => {
    const theme = makeTheme(dark);
    for (let ses in sessions) {
      sessions[ses].term.setOption('theme', theme);
    }
    if (container.current) {
      container.current.style.backgroundColor = theme.background || '';
    }
  }, [dark, sessions]);

  //  on selected change, maybe setup the term, or put it into the container
  //
  useEffect(() => {
    let ses = sessions[selected];

    //  initialize terminal
    //
    if (!ses) {
      //  set up terminal
      //
      let term = new Terminal(termConfig);
      term.setOption('theme', makeTheme(dark));
      const fit = new FitAddon();
      term.loadAddon(fit);

      //  start mouse reporting
      //
      term.write(csi('?9h'));

      //  set up event handlers
      //
      term.onData((e) => onInput(selected, e));
      term.onBinary((e) => onInput(selected, e));
      //TODO  term.onResize

      ses = { term, fit };

      //  open subscription
      //
      //TODO  start default session alongside other landscape subscriptions,
      //      once subscription refactor is in.
      api.subscribe('/session/'+selected, 'PUT', api.ship, 'herm',
        (e) => {
          onBlit(selected, e.data);
        },
        (err) => {  //  fail
          console.log('sub error', selected, err);
          //TODO  resubscribe
        },
        () => {  //  quit
          //TODO  resubscribe
        }
      );
    }

    if (container.current && !container.current.contains(ses.term.element || null)) {
      ses.term.open(container.current);
      ses.fit.fit();  //TODO  if not default, send %blew
      ses.term.focus();
    }

    set(state => { state.sessions[selected] = ses; });

    return () => {
      //TODO  unload term from container
      //      but term.dispose is too powerful? maybe just empty the container?
    }
  }, [set, sessions[selected], container.current]);

  return (
    <>
      <Helmet defer={false}>
        <title>{ props.notificationsCount ? `(${String(props.notificationsCount) }) `: '' }Landscape</title>
      </Helmet>
      <Box
        width='100%'
        height='100%'
        display='flex'
      >
        <Col
            width='100%'
            minHeight='0'
            color='washedGray'
            borderRadius='2'
            mx={['0','3']}
            mb={['0','3']}
            border={['0','1']}
            //@ts-ignore  //NOTE  fix in indigo Soon™
            ref={container}  //TODO  might somehow be undefined?
          >
        </Col>
      </Box>
    </>
  );
}
