var BigInteger = require('jsbn').BigInteger;

function Noun() {
  this._mug = 0;
}

Noun.prototype.loob = function () {
  throw new Error("Bail");
};

Noun.prototype.toString = function() {
  var parts = [];
  this.pretty(parts, false);
  return parts.join('');
};

Noun.prototype.mug = function () {
  if ( 0 === this._mug ) {
    this._mug = this.calculateMug();
  }
  return this._mug;
};

Noun.prototype.mugged = function () {
  return 0 !== this._mug;
};

Noun.prototype.deep = false;
Noun.prototype.bump = function () {
  throw new Error("Bail");
}

Noun.prototype.equals = function(o) {
  if ( this === o ) {
    return true;
  }

  if ( this instanceof Cell ) {
    if ( o instanceof Cell) {
      return this.unify(o);
    }
    else {
      return false;
    }
  }
  else {
    if ( o instanceof Cell ) {
      return false;
    }
    else if (0 === this.number.compareTo(o.number)) {
      o.number = this.number;
      return true;
    }
    else {
      return false;
    }
  }
};


function _mug_fnv(has_w) {
  return Math.imul(has_w, 16777619);
}

function _mug_out(has_w) {
  return (has_w >>> 31) ^ (has_w & 0x7fffffff);
}

function _mug_both(lef_w, rit_w) {
  var bot_w = _mug_fnv(lef_w ^ _mug_fnv(rit_w));
  var out_w = _mug_out(bot_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_both(lef_w, ++rit_w);
  }
}

function Cell(head, tail) {
  Noun.call(this);
  this.head = head;
  this.tail = tail;
}
Cell.prototype = Object.create(Noun.prototype);
Cell.prototype.constructor = Cell;
Cell.prototype.deep = true;

Cell.prototype.pretty = function(out, tail) {
  if ( !tail ) {
    out.push('[');
  }
  this.head.pretty(out, false);
  out.push(' ');
  this.tail.pretty(out, true);
  if ( !tail ) {
    out.push(']');
  }
};

Cell.prototype.calculateMug = function() {
  return _mug_both(this.head.mug(), this.tail.mug());
};

Cell.prototype.unify = function(o) {
  if ( this === o ) {
    return true;
  }

  if ( o.mugged() ) {
    if ( this.mugged() ) {
      if ( this.mug() != o.mug() ) {
        return false;
      }
    }
    else {
      return o.unify(this);
    }
  }

  if ( this.head.equals(o.head) ) {
    o.head = this.head;
    if ( this.tail.equals(o.tail) ) {
      o._mug = this._mug;
      o.tail = this.tail;
      return true;
    }
  }

  return false;
};

function Atom(number) {
  Noun.call(this);
  this.number = number;
}
Atom.prototype = Object.create(Noun.prototype);
Atom.prototype.constructor = Atom;

var small = new Array(256);
(function() {
  var i, bi;
  for ( i = 0; i < 256; ++i ) {
    bi = new BigInteger();
    bi.fromInt(i);
    small[i] = new Atom(bi);
  }
})();

var fragCache = {
  0: function(a) {
    throw new Error("Bail");
  },
  1: function(a) {
    return a;
  },
};
var one = small[1];
Noun.fragmenter = function(a) {
  var s = a.shortCode();
  if ( fragCache.hasOwnProperty(s) ) {
    return fragCache[s];
  }
  else {
    for ( var parts = ['a']; !one.equals(a); a = a.mas() ) {
      parts.push( ( 2 === a.cap().valueOf() ) ? 'head' : 'tail' );
    }
    return fragCache[s] = new Function('a', 'return ' + parts.join('.') + ';');
  }
}

Noun.prototype.at = function(a) {
  return Noun.fragmenter(a)(this);
};

var shortBi = new BigInteger();
shortBi.fromInt(65536);

Atom.prototype.bytes = function() {
  var bytes = this.number.toByteArray();
  var r = [];
  for ( var i = bytes.length-1; i >= 0; --i ) {
    r.push(bytes[i]&0xff);
  }
  return r;
}

Atom.cordToString = function(c) {
  var bytes = c.bytes(),
      chars = [];

  for ( var i = 0; i < bytes.length; ++i ) {
    chars.push(String.fromCharCode(bytes[i]));
  }
  return chars.join('');
};

Atom.prototype.pretty = function(out, tail) {
  if ( this.number.compareTo(shortBi) < 0 ) {
    return out.push(this.number.toString(10));
  }
  else {
    var tap = [], isTa = true, isTas = true, bytes = this.number.toByteArray();
    for ( var i = bytes.length - 1; i >= 0; --i) {
      var c = bytes[i];
      if ( isTa && ((c < 32) || (c > 127)) ) {
        isTa = false;
        isTas = false;
        break;
      }
      else if ( isTas && !((c > 47 && c < 58) ||  // digits
                           (c > 96 && c < 123) || // lowercase letters
                            c === 45) ) {         // -
        isTas = false;
      }
      tap.push(String.fromCharCode(c));
    }
    if ( isTas ) {
      out.push('%');
      out.push.apply(out, tap);
    }
    else if ( isTa ) {
      out.push("'");
      out.push.apply(out, tap);
      out.push("'");
    }
    else {
      out.push("0x");
      out.push(this.number.toString(16));
    }
  }
};

Atom.prototype.loob = function() {
  switch ( this.number.intValue() ) {
    case 0:
      return true;
    case 1:
      return false;
    default:
      throw new Error("Bail");
  }
};

Atom.prototype.bump = function() {
  return new Atom(this.number.add(BigInteger.ONE));
};

var ida  = i(1);
var heda = i(2);
var tala = i(3);

Atom.prototype.cap = function() {
  switch (this.number.intValue()) {
    case 0:
    case 1:
      throw new Error("Bail");
    default:
    return this.number.testBit(this.number.bitLength() - 2) ? tala : heda;
  }
};

Atom.prototype.mas = function() {
  switch (this.number.intValue()) {
    case 0:
    case 1:
      throw new Error("Bail");
    case 2:
    case 3:
      return ida;
    default:
      var n = this.number;
      var l = n.bitLength() - 2;
      var addTop = new BigInteger();
      addTop.fromInt(1 << l);
      var mask = new BigInteger();
      mask.fromInt((1 << l)-1);
      return new Atom(n.and(mask).xor(addTop));
  }
};

Atom.prototype.calculateMug = function() {
	var a = this.number.toByteArray();
	var b, c, d, e, f, bot;
	for ( e = a.length - 1, b = (2166136261|0); ; ++b ) {
    c = b;
    bot = ( 0 === a[0] ) ? 1 : 0;
		for ( d = e; d >= bot; --d ) {
      c = _mug_fnv(c ^ (0xff & a[d]));
		}
    f = _mug_out(c);
    if ( 0 !== f ) {
      return f;
    }
	}
};

Atom.prototype.shortCode = function() {
  return this.number.toString(36); // max supported by BigInteger
};

function s(str, radix) {
	return new Atom(new BigInteger(str, radix));
}

function i(num) {
  if ( num < 256 ) {
    return small[num];
  }
  else {
    var bi = new BigInteger();
    bi.fromInt(num);
    return new Atom(bi);
  }
}

function m(str) {
  var i, j, octs = new Array(str.length);
  for ( i = 0, j = octs.length - 1; i < octs.length; ++i, --j ) {
    octs[j] = (str.charCodeAt(i) & 0xff).toString(16);
  }
  return new Atom(new BigInteger(octs.join(''), 16))
}

function dwim(a) {
  var n = (arguments.length === 1 ? a : Array.apply(null, arguments));
	if ( n instanceof Noun ) {
		return n;
	}
	if ( typeof n === "number" ) {
		return i(n);
	}
	else if ( Array.isArray(n) ) {
		var cel = new Cell(dwim(n[n.length-2]), dwim(n[n.length-1]));
		for ( var j = n.length-3; j >= 0; --j ) {
			cel = new Cell(dwim(n[j]), cel);
		}
		return cel;
	}
	else if ( typeof n === "string" ) {
    return m(n);
	}
  console.log('what do you mean??', typeof n, n instanceof Noun, n.toString(), n);
}

Atom.prototype.valueOf = function() {
	return this.number.bitLength() <= 32
		? this.number.intValue()
		: this.number.toString();
};

//TODO  consider doing the dynamic args thing dwim does
const frond = function(opts) { // {tag: string, get: function(noun)}[]
  return function(noun) {
    if (!(noun instanceof Cell && noun.head instanceof Atom)) {
      throw new Error('frond: noun not cell');
    }
    const tag = Atom.cordToString(noun.head);
    for (let i = 0; i < opts.length; i++) {
      if (tag === opts[i].tag) {
        return { [tag]: opts[i].get(noun.tail) };
      }
    }
    throw new Error('frond: unknown tag', tag);
  };
}

const pairs = function(cels) { // {nom: string, get: function(noun)}[]
  return function(noun) {
    let i = 0;
    let o = {};
    while(i < cels.length-1) {
      if (!(noun instanceof Cell)) {
        throw new Error('pairs: noun too shallow');
      }
      o[cels[i].nom] = cels[i].get(noun.head);
      noun = noun.tail;
      i++;
    }
    o[cels[i].nom] = cels[i].get(noun);
    return o;
  };
}

const pair = function(na, ga, nb, gb) {
  return pairs([{nom: na, get: ga}, {nom: nb, get: gb}]);
}

const bucwut = function(opts) { // function(noun)[]
    return function(noun) {
      for (let i = 0; i < opts.length; i++) {
        try {
          const res = opts[i](noun);
          return res;
        } catch(e) {
          continue;
        }
      }
      throw new Error('bucwut: no matches');
    }
}

const array = function(item) { // function(noun)
  return function(noun) {
    let a = [];
    while (noun instanceof Cell) {
      a.push(item(noun.head));
      noun = noun.tail;
    }
    return a;
  }
}

const tree = function(item) { // function(noun)
  return function(noun) {
    let a = [];
    if (noun instanceof Cell) {
      if (!(noun.tail instanceof Cell)) {
        throw new Error('tree: malformed');
      }
      a = [
        ...a,
        item(noun.head),
        ...tree(item)(noun.tail.head),
        ...tree(item)(noun.tail.tail),
      ];
    }
    return a;
  }
}

const cord = function(noun) {
  if (!(noun instanceof Atom)) {
    throw new Error('cord: noun not atom');
  }
  return Atom.cordToString(noun);
}

const numb = function(noun) {
  if (!(noun instanceof Atom)) {
    throw new Error('numb: noun not atom');
  }
  return noun.valueOf();
}

const loob = function(noun) {
  return noun.loob();
}

const nill = function(noun) {
  if (!(noun instanceof Atom && noun.number.intValue() === 0)) {
    throw new Error('nill: not null');
  }
  return null;
}

const path = array(cord);

module.exports = {
	dwim: dwim,
	Noun: Noun,
	Cell: Cell,
	Atom: {
		Atom: Atom,
		yes:  i(0),
		no:   i(1),
    fromMote:   m,
		fromInt:    i,
		fromString: s,
	},
  enjs: {
    //TODO  ship, tape, tank, time, unit
    frond, pairs, array, cord, numb, path, pair, bucwut, nill, tree, loob
  },
  dejs: {
    //TODO  dwim, list, ship, tape, time
  }
};
