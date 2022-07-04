/* keys can be any noun, values aren't constrained */

function Slot() {
}

function Node() {
  Slot.call(this);
  this.slots = new Array(32);
}
Node.prototype = Object.create(Slot.prototype);
Node.prototype.constructor = Node;

Node.prototype.insert = function(key, val, lef, rem) {
  var inx;
  lef -= 5;
  inx = rem >>> lef;
  rem &= ((1 << lef) - 1);

  this.slots[inx] = ( undefined === this.slots[inx] )
                  ? new Single(key, val)
                  : this.slots[inx].insert(key, val, lef, rem);
  return this;
};

Node.prototype.get = function(key, lef, rem) {
  var inx, sot;
  lef -= 5;
  inx = rem >>> lef;
  rem &= ((1 << lef) - 1);
  sot = this.slots[inx];

  return ( undefined === sot ) ? undefined : sot.get(key, lef, rem);
};

function Bucket() {
  this.singles = [];
}
Bucket.prototype = Object.create(Slot.prototype);
Bucket.prototype.constructor = Bucket;

Bucket.prototype.insert = function(key, val, lef, rem) {
  var s, a = this.singles;

  for ( var i = 0; i < a.length; ++i ) {
    s = a[i];
    if ( s.key.equals(key) ) {
      s.val = val;
      return this;
    }
  }
  a.push(new Single(key, val));
  return this;
};

Bucket.prototype.get = function(key, lef, rem) {
  var s, a = this.singles;

  for ( var i = 0; i < a.length; ++i ) {
    s = a[i];
    if ( s.key.equals(key) ) {
      return s.val;
    }
  }

  return undefined;
};

function Single(key, val) {
  Slot.call(this);
  this.key = key;
  this.val = val;
}
Single.prototype = Object.create(Slot.prototype);
Single.prototype.constructor = Single;

Single.prototype.insert = function(key, val, lef, rem) {
  if ( this.key.equals(key) ) {
    this.val = val;
    return this;
  }
  else {
    var n, rom = this.key.mug() & ((1 << lef) - 1);

    if ( lef > 0 ) {
      n = new Node();
    }
    else {
      n = new Bucket();
    }
    n.insert(this.key, this.val, lef, rom);
    n.insert(key, val, lef, rem);
    return n;
  }
};

Single.prototype.get = function(key, lef, rem) {
  if ( this.key.equals(key) ) {
    return this.val;
  }
  else {
    return undefined;
  }
};

function NounMap() {
  this.slots = new Array(64);
}

NounMap.prototype.insert = function(key, val) {
  var m    = key.mug();
  var inx  = m >>> 25;
  var sot  = this.slots;
  if ( undefined === sot[inx] ) {
    sot[inx] = new Single(key, val);
  }
  else {
    var rem  = m & ((1 << 25) - 1);
    sot[inx] = sot[inx].insert(key, val, 25, rem);
  }
};

NounMap.prototype.get = function(key) {
  var m = key.mug();
  var inx = m >>> 25;
  var sot = this.slots[inx];
  if ( undefined === sot ) {
    return undefined;
  }
  else {
    var rem  = m & ((1 << 25) - 1);
    return sot.get(key, 25, rem);
  }
};

module.exports = {
  NounMap: NounMap
};
