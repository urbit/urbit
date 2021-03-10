import React, {
  Component,
  useState,
  useEffect,
  useRef,
  useCallback
} from 'react';
import { Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Terminal, ITerminalOptions } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import { saveAs } from 'file-saver';

import { Box, Col } from '@tlon/indigo-react';

import './css/custom.css';
import GlobalApi from '~/logic/api/global';
import { Belt } from '~/logic/api/term';
import { Blit, Stye, Stub, Tint, Deco } from '~/types/term-update';

import bel from '../../../logic/lib/bel';

type TermAppProps = {
  api: GlobalApi;
  ship: string;
  notificationsCount: number;
}

type Sessions = {
  [id: string]: {
    term: Terminal,
    fit: FitAddon,
    sub: boolean
  };
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
  theme: { //TODO  vary with landscape theme?
    foreground: 'black',
    background: 'white',
    cursor: 'black',
    cursorAccent: 'white',
    //TODO  selection color
  },
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
  const [sessions, setSessions] = useState<Sessions>({});
  const [selected, setSelected] = useState('default');  //TODO  allow switching

  const onSlog = useCallback((slog) => {
    if (!sessions['default']) {
      console.log('default session mia!', 'slog:', slog);
      return;
    }
    const term = sessions['default'].term;

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
  }, []);

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
    else if ('lin' in blit) {
      out += blit.lin.join('');
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

    sessions[ses].term.write(out);
  }, []);

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

  const onInput = useCallback((ses: string, e) => {
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
        belts.push({ key: { mod: 'ctl', key: String.fromCharCode(96 + c) } });
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
          belts.push({ key: { mod: 'met', key: e[0] } });
        }
        else if (c === 46) {  //  .
          belts.push({ key: { mod: 'met', key: '.' } });
        }
        else if (c === 8 || c === 127) {
          belts.push({ key: { mod: 'met', key: { bac: null } } });
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

  //  when switching sessions, initialize if necessary
  //
  useEffect(() => {
    //  initialize terminal
    //
    if (!sessions[selected]) {
      //  set up terminal
      //
      console.log('new term!', selected);
      let term = new Terminal(termConfig);
      const fit = new FitAddon();
      term.loadAddon(fit);

      //  start mouse reporting
      //
      term.write(csi('?9h'));

      //  set up event handlers
      //
      term.onData((e) => onInput(selected, e));
      term.onBinary((e) => onInput(selected, e));

      //TODO  open subscription

      //  persist in state
      //
      sessions[selected] = { term, fit, sub: false };
      setSessions(sessions);
    }

    if (container.current) {
      sessions[selected].term.open(container.current);  //TODO  once
      sessions[selected].fit.fit();  //TODO  if not default, send %blew
    }

    console.log('need sub?', selected, sessions[selected].sub);
    if (!sessions[selected].sub) {
      //TODO  BaseSubscribe?
      let ses = selected;
      if (ses === 'default') ses = '';
      //TODO  get from a SubscriptionBase somewhere?
      console.log('starting sub', selected);
      api.subscribe('/session/', 'PUT', api.ship, 'herm',
        (e) => {
          onBlit(selected, e.data);
        },
        (err) => {  //  fail
          console.log(err);
          //TODO  resubscribe
        },
        () => {  //  quit
          //TODO  resubscribe
        });
      //TODO  set in state
    }
  //TODO  can we do sessions[selected]?
  }, [container.current, sessions, selected, onBlit]);

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
            ref={container}  //TODO  might somehow be undefined?
          >
        </Col>
      </Box>
    </>
  );
}
