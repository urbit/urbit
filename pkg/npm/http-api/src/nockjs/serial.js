var noun    = require('./noun.js'),
    list    = require('./list.js'),
    Cell    = noun.Cell,
    bits    = require('./bits.js'),
    zero    = noun.Atom.yes,
    one     = noun.Atom.no,
    i       = noun.Atom.fromInt,
    two     = i(2),
    three   = i(3),
    NounMap = require('./hamt.js').NounMap;

function rub(a, b) {
	var c, d, e, w, x, y, z, p, q, m;

	m = bits.add(a, i(bits.met(0, b)));
	x = a;

	while ( zero.equals(bits.cut(zero, x, one, b)) ) {
		y = bits.add(one, x);

		//  Sanity check: crash if decoding more bits than available
		if ( bits.gth(x, m) ) {
			throw new Error("Bail");
		}

		x = y;
	}

	if ( a.equals(x) ) {
		return new Cell(one, zero);
	}

	c = bits.sub(x, a);
	d = bits.add(x, one);

	x = bits.dec(c);
	y = bits.bex(x);
	z = bits.cut(zero, d, x, b);

	e = bits.add(y, z);
	w = bits.add(c, c);
	y = bits.add(w, e);
	z = bits.add(d, x);

	p = bits.add(w, e);
	q = bits.cut(zero, z, e, b);

	return new Cell(p, q);
}

function cue_in(m, a, b) {
  var x,c,p,q,l,u,v,w,y,p,q,d,x;

  if ( zero.equals(bits.cut(zero, b, one, a)) ) {
    x = bits.add(b, one);
    c = rub(x, a);
    p = bits.add(c.head, one);
    q = c.tail;
    m.insert(b, q);
  }
  else {
    c = bits.add(two, b);
    l = bits.add(one, b);

    if ( zero.equals(bits.cut(zero, l, one, a)) ) {
      u = cue_in(m, a, c);
      x = bits.add(u.head, c);
      v = cue_in(m, a, x);
      w = new Cell(u.tail.head, v.tail.head);
      y = bits.add(u.head, v.head);
      p = bits.add(two, y);
      q = w;
      m.insert(b, q);
    }
    else {
      d = rub(c, a);
      x = m.get(d.tail);

      if ( undefined === x ) {
        throw new Error("Bail");
      }

      p = bits.add(two, d.head);
      q = x;
    }
  }
  return new Cell(p, new Cell(q, zero));
}

function cue(a) {
  return cue_in(new NounMap(), a, zero).tail.head;
}

function mat(a) {
	if ( zero.equals(a) ) {
		return noun.dwim(1, 1);
	}
  else {
		var b = noun.dwim(bits.met(0, a)),
		    c = noun.dwim(bits.met(0, b)),
		    u = bits.dec(c),
        v = bits.add(c, c),
        x = bits.end(zero, u, b),
        w = bits.bex(c),
        y = bits.lsh(zero, u, a),
        z = bits.mix(x, y),
        p = bits.add(v, b),
        q = bits.cat(zero, w, z);
    return noun.dwim(p, q);
	}       
}

function _jam_in_pair(m, h_a, t_a, b, l) {
	var w = noun.dwim([2, 1], l),
      x = bits.add(two, b),
      d = _jam_in(m, h_a, x, w),
      y = bits.add(x, d.head),
      e = _jam_in(m, t_a, y, d.tail.head),
      z = bits.add(d.head, e.head);

  return noun.dwim(bits.add(two, z), e.tail.head, zero);
}

function _jam_in_ptr(m, u_c, l) {
  var d = mat(u_c),
      x = bits.lsh(zero, two, d.tail),
      y = bits.add(two, d.head);

  return noun.dwim(y, [[y, bits.mix(three, x)], l], zero);
}

function _jam_in_flat(m, a, l) {
	var d = mat(a),
      x = bits.add(one, d.head);

  return noun.dwim(x, [[x, bits.lsh(zero, one, d.tail)], l], zero);
}

function _jam_in(m, a, b, l) {
  var x, c = m.get(a);

  if ( undefined == c ) {
    m.insert(a, b);
    return a.deep ?
      _jam_in_pair(m, a.head, a.tail, b, l) :
      _jam_in_flat(m, a, l);
  }
  else if ( !a.deep && bits.met(0, a) <= bits.met(0, c) ) {
    return _jam_in_flat(m, a, l);
  }
  else {
    return _jam_in_ptr(m, c, l);
  }
}

function jam(n) {
  var x = _jam_in(new NounMap(), n, zero, zero),
      q = list.flop(x.tail.head);

  return bits.can(zero, q);
}

module.exports = {
  cue: cue,
  mat: mat,
  jam: jam
};
