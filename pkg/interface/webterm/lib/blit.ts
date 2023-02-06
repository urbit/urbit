import { Terminal } from 'xterm';
import { saveAs } from 'file-saver';
import { Blit, Stub, Stye } from '@urbit/api/term';
import { stye } from '../lib/stye';

export const csi = (cmd: string, ...args: number[]) => {
  return '\x1b[' + args.join(';') + cmd;
};

export const showBlit = (term: Terminal, blit: Blit) => {
  let out = '';

  if ('mor' in blit) {
    return blit.mor.map(b => showBlit(term, b));
  } else if ('bel' in blit) {
    out += '\x07';
  } else if ('clr' in blit) {
    term.clear();
    out += csi('u');
  } else if ('hop' in blit) {
    if (typeof blit.hop === 'number') {
      out += csi('H', term.rows, blit.hop + 1);
    } else {
      out += csi('H', blit.hop.y + 1, blit.hop.x + 1);
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
  //  move cursor to bottom left of the scroll region,
  //  print a newline to move everything up a line,
  //  set text to grey,
  //  print the slog,
  //  restore color, scroll region, and cursor.
  //
  term.write(csi('r', 1, term.rows - 1)
           + csi('H', term.rows - 1, 1)
           + '\n'
           + csi('m', 90)
           + slog
           + csi('m', 0)
           + csi('r')
           + csi('u'));
};

export const hasBell = (blit: Blit) => {
  if ('bel' in blit) {
    return true;
  } else if ('mor' in blit) {
    return blit.mor.some(hasBell);
  } else {
    return false;
  }
};

//  debug rendering
//NOTE  doesn't behave nicely in the presence of eob %nel blits,
//      because those aren't idempotent

const blotStye: Stye = { deco: [], back: { r: 255, g: 0, b: 255 }, fore: 'k' };
const blitToBlot = (blit: Blit): Blit => {
  if ('mor' in blit) {
    return { mor: blit.mor.map(blitToBlot) };
  } else if ('put' in blit) {
    return { klr: [{ text: blit.put, stye: blotStye }] };
  } else if ('klr' in blit) {
    return { klr: blit.klr.map((s: Stub) => {
      return { text: s.text, stye: blotStye };
    }) };
  } else {
    return blit;
  }
};

const queue: {term: Terminal, blit: Blit}[] = [];
const renderFromQueue = () => {
  const next = queue.shift();
  if (!next) {
    return;
  }
  showBlit(next.term, next.blit);
  if (0 === queue.length) {
    return;
  }
  setTimeout(renderFromQueue, 200);
};

export const showBlitDebug = (term: Terminal, blit: Blit) => {
  const blot = blitToBlot(blit);
  if (0 === queue.length) {
    showBlit(term, blot);
    queue.push({ term, blit });
    setTimeout(renderFromQueue, 200);
  } else {
    queue.push({ term, blit: blot });
    queue.push({ term, blit });
  }
};
