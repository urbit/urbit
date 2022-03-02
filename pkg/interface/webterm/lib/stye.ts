import { Deco, Stye, Tint } from '@urbit/api/term';

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

export const stye = (s: Stye) => {
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
