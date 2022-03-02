import { Terminal } from 'xterm';
import { saveAs } from 'file-saver';
import { Blit, Stub } from '@urbit/api/term';
import { stye } from '../lib/stye';

export const csi = (cmd: string, ...args: number[]) => {
  return '\x1b[' + args.join(';') + cmd;
};

export const showBlit = (term: Terminal, blit: Blit) => {
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

export const showSlog = (term: Terminal, slog: string) => {
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
