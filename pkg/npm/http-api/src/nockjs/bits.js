var noun = require('./noun.js'),
    BigInteger = require('jsbn').BigInteger,
    zero = noun.Atom.yes,
    one  = noun.Atom.no;

// a is native, returns native
function met(a, b) {
  var bits = b.number.bitLength(),
      full = bits >>> a,
      part = (full << a) !== bits;

  return part ? full + 1 : full;
}

function gth(a, b) {
  return a.number.compareTo(b.number) > 0;
}

function lth(a, b) {
  return a.number.compareTo(b.number) < 0;
}

function gte(a, b) {
  return a.number.compareTo(b.number) >= 0;
}

function lte(a, b) {
  return a.number.compareTo(b.number) <= 0;
}

function add(a, b) {
  return new noun.Atom.Atom(a.number.add(b.number));
}

function dec(a) {
  return sub(a, one);
}

function bex(a) {
  return new noun.Atom.Atom(BigInteger.ONE.shiftLeft(a.number.intValue()));
}

function sub(a, b) {
  var r = a.number.subtract(b.number);
  if ( r.signum < 0 ) {
    throw new Error("subtract underflow");
  }
  else {
    return new noun.Atom.Atom(r);
  }
}

function lsh(a, b, c) {
  var bits = b.number.shiftLeft(a.number.intValue()).intValue();
  return new noun.Atom.Atom(c.number.shiftLeft(bits));
}

function rsh(a, b, c) {
  var bits = b.number.shiftLeft(a.number.intValue()).intValue();
  return new noun.Atom.Atom(c.number.shiftRight(bits));
}

// to/from little-endian 32-bit word array, as used in vere
// TODO: efficiency is horrible here, but can be improved using internals
function bytesToWords(bytes) {
  var len   = bytes.length,
      trim  = len % 4;
  var i, b, w;

	if ( trim > 0 ) {
    len += (4-trim);
    for ( i = 0; i < trim; ++i ) {
      bytes.push(0);
    }
	}

  var size = len >> 2;
  var words = new Array(size);
  for ( i = 0, b = 0; i < size; ++i ) {
    w =  (bytes[b++] << 0)  & 0x000000FF;
    w ^= (bytes[b++] << 8)  & 0x0000FF00;
    w ^= (bytes[b++] << 16) & 0x00FF0000;
    w ^= (bytes[b++] << 24) & 0xFF000000;
    words[i] = w;
  }
  return words;
};

function wordsToBytes(words) {
  var buf = [];
  var w, i, b;
  for ( i = 0, b = 0; i < words.length; ++i ) {
    w = words[i];
    buf[b++] = 0xff & (w & 0x000000FF);
    buf[b++] = 0xff & ((w & 0x0000FF00) >>> 8);
    buf[b++] = 0xff & ((w & 0x00FF0000) >>> 16);
    buf[b++] = 0xff & ((w & 0xFF000000) >>> 24);
  }
  // or here. one of the 'get rid of extra zeros' functions.
  while ( buf[--b] === 0 ) {
    buf.pop();
  }
  return buf;
};

function bytesToAtom(bytes) {
  var byt, parts = [];
  for ( var i = bytes.length - 1; i >= 0; --i ) {
    byt = bytes[i] & 0xff;
    parts.push(byt < 16 ? ("0" + byt.toString(16)) : byt.toString(16));
  }
  return new noun.Atom.Atom(new BigInteger(parts.join(''), 16));
}

function atomToBytes(atom) {
  return atom.bytes();
}

function atomToWords(atom) {
  return bytesToWords(atomToBytes(atom));
}

function wordsToAtom(words) {
  return bytesToAtom(wordsToBytes(words));
}

var malt = wordsToAtom;

// XX: INTERNAL
function slaq(bloq, len) {
  return new Array(((len << bloq) + 31) >>> 5);
}

// src is atom, all others native
function chop(met, fum, wid, tou, dst, src) {
  var buf = atomToWords(src),
      len = buf.length,
      i, j, san, mek, baf, bat, hut, san,
      wuf, wut, waf, raf, wat, rat, hop;

  if ( met < 5 ) {
    san = 1 << met;
    mek = ((1 << san) - 1);
    baf = fum << met;
    bat = tou << met;

    for ( i = 0; i < wid; ++i ) {
      waf = baf >>> 5;
      raf = baf & 31;
      wat = bat >>> 5;
      rat = bat & 31;
      hop = ((waf >= len) ? 0 : buf[waf]);
      hop = ((hop >>> raf) & mek);
      dst[wat] ^= hop << rat;
      baf += san;
      bat += san;
    }
  }
  else {
    hut = met - 5;
    san = 1 << hut;

    for ( i = 0; i < wid; ++i ) {
      wuf = (fum + i) << hut;
      wut = (tou + i) << hut;

      for ( j = 0; j < san; ++j ) {
        dst[wut + j] ^= ((wuf + j) >= len)
                      ? 0
                      : buf[wuf + j];
      }
    }
  }
}

function cut(a, b, c, d) {
  var ai = a.number.intValue(),
      bi = b.number.intValue(),
      ci = c.number.intValue();

  var len = met(ai, d);
  if ( zero.equals(c) || bi >= len ) {
    return zero;
  }
  if ( bi + ci > len ) {
    ci = len - b;
  }
  if ( 0 === bi && ci === len ) {
    return d;
  }
  else {
    var sal = slaq(ai, ci);
    chop(ai, bi, ci, 0, sal, d);
    return malt(sal);
  }
}

var maxCat = noun.Atom.fromInt(0xffffffff);
var catBits = noun.Atom.fromInt(32);

function end(a, b, c) {
  if ( gth(a, catBits) ) {
    throw new Error("Fail");
  }
  else if ( gth(b, maxCat) ) {
    return c;
  }
	else {
    var ai = a.number.intValue(),
        bi = b.number.intValue(),
       len = met(ai, c);

    if ( 0 === bi ) {
      return zero;
    }
    else if ( bi >= len ) {
      return c;
    }
    else {
      var sal = slaq(ai, bi);
      chop(ai, 0, bi, 0, sal, c);
      return malt(sal);
    }
	}
}

function mix(a, b) {
  return new noun.Atom.Atom(a.number.xor(b.number));
}

function cat(a, b, c) {
	if ( gth(a, catBits) ) {
		throw new Error("Fail");
	}
	else {
		var ai = a.number.intValue(),
       lew = met(ai, b),
       ler = met(ai, c),
       all = lew + ler;

    if ( 0 === all ) {
      return zero;
    }
    else {
      var sal = slaq(ai, all);
      chop(ai, 0, lew, 0, sal, b);
      chop(ai, 0, ler, lew, sal, c);
      return malt(sal);
    }
	}
}

function can(a, b) {
  if ( gth(a, catBits) ) {
    throw new Error("Fail");
  }
  else {
    var ai = a.number.intValue(),
      tot = 0,
      cab = b,
      pos, i_cab, pi_cab, qi_cab;

    // measure
    while ( true ) {
      if ( zero.equals(cab) ) {
        break;
      }
      if ( !cab.deep ) {
        throw new Error("Fail");
      }
      i_cab = cab.head;
      if ( !i_cab.deep ) {
        throw new Error("Fail");
      }
      pi_cab = i_cab.head;
      qi_cab = i_cab.tail;
      if ( gth(pi_cab, maxCat) ) {
        throw new Error("Fail");
      }
      if ( qi_cab.deep ) {
        throw new Error("Fail");
      }
      if ( (tot + pi_cab) < tot ) {
        throw new Error("Fail");
      }
      tot += pi_cab;
      cab = cab.tail;
    }
    if ( 0 === tot ) {
      return zero;
    }
    var sal = slaq(ai, tot);

    // chop the list atoms in
    cab = b;
    pos = 0;
    while ( !zero.equals(cab) ) {
      i_cab  = cab.head;
      pi_cab = i_cab.head.number.intValue();
      qi_cab = i_cab.tail;

      chop(ai, 0, pi_cab, pos, sal, qi_cab);
      pos += pi_cab;
      cab = cab.tail;
    }
    return malt(sal);
  }
}

module.exports = {
  met: met,
  cut: cut,
	add: add,
	sub: sub,
  dec: dec,
  gth: gth,
  lth: lth,
  gte: gte,
  lte: lte,
  bex: bex,
  lsh: lsh,
  rsh: rsh,
  end: end,
  mix: mix,
  cat: cat,
  can: can,
  bytesToWords: bytesToWords,
  wordsToBytes: wordsToBytes,
  bytesToAtom: bytesToAtom,
  atomToBytes: atomToBytes,
  atomToWords: atomToWords,
  wordsToAtom: wordsToAtom,
};
