var noun = require('./noun.js'),
    NounMap = require('./hamt.js').NounMap,
    list = require('./list.js'),
    Noun = noun.Noun,
    Atom = noun.Atom.Atom,
    Cell = noun.Cell,
    MEMO = noun.dwim("memo"),
    SLOG = noun.dwim("slog"),
    FAST = noun.dwim("fast"),
    SPOT = noun.dwim("spot"),
    MEAN = noun.dwim("mean"),
    HUNK = noun.dwim("hunk"),
    LOSE = noun.dwim("lose");

function Statement() {
}

function Block() {
  Statement.call(this);
  this.statements = [];
}
Block.prototype = Object.create(Statement.prototype);
Block.prototype.constructor = Block;

Block.prototype.append = function(st) {
  this.statements.push(st);
}

Block.prototype.toJs = function() {
  var sts   = this.statements;
  var parts = new Array(sts.length);
  for ( var i = 0; i < sts.length; ++i) {
    parts[i] = sts[i].toJs();
  }
  return parts.join('');
};

function Assignment(name, expr) {
  Statement.call(this);
  this.name = name;
  this.expr = expr;
}
Assignment.prototype = Object.create(Statement.prototype);
Assignment.prototype.constructor = Assignment;

Assignment.prototype.toJs = function() {
  return "var " + this.name + " = " + this.expr.toJs() + ";";
}

function Expression() {
}

function Cons(head, tail) {
  Expression.call(this);
  this.head = head;
  this.tail = tail;
}
Cons.prototype = Object.create(Expression.prototype);
Cons.prototype.constructor = Cons;

Cons.prototype.toJs = function() {
  return "context.cons(" + this.head + ", " + this.tail + ")";
};

function Frag(axis, name) {
  Expression.call(this);
  this.axis = axis;
  this.name = name;
}
Frag.prototype = Object.create(Expression.prototype);
Frag.prototype.constructor = Frag;

Frag.prototype.toJs = function() {
  var parts = [this.name];
  for ( var ax = this.axis; ax > 1; ax = ax.mas() ) {
    parts.push( ( 2 === ax.cap().valueOf() ) ? "head" : "tail" );
  }
  return parts.join(".");
};

function Bail() {
  Statement.call(this);
}
Bail.prototype = Object.create(Statement.prototype);
Bail.prototype.constructor = Bail;

Bail.prototype.toJs = function() {
  return "throw new Error(\"Bail\")";
};

function Identity(name) {
  Expression.call(this);
  this.name = name;
}
Identity.prototype = Object.create(Expression.prototype);
Identity.prototype.constructor = Identity;

Identity.prototype.toJs = function() {
  return this.name;
};

function Constant(index) {
  Expression.call(this);
  this.index = index;
}
Constant.prototype = Object.create(Expression.prototype);
Constant.prototype.constructor = Constant;

Constant.prototype.toJs = function() {
  return "constants[" + this.index + "]";
};

function Nock(subject, formula, tail) {
  Expression.call(this);
  this.subject = subject;
  this.formula = formula;
  this.tail    = tail;
}
Nock.prototype = Object.create(Expression.prototype);
Nock.prototype.constructor = Nock;

Nock.prototype.toJs = function() {
  var f = this.formula;
  var targetCode = "(" + f + ".hasOwnProperty('target') ? " + f + 
    ".target : (" + f + ".target = context.compile(" + this.formula + ")))";
  return this.tail ?
    "context.trampoline(" + targetCode + ", " + this.subject + ")" :
    targetCode + "(" + this.subject + ")";
};

function Deep(name) {
  Expression.call(this);
  this.name = name;
}
Deep.prototype = Object.create(Expression.prototype);
Deep.prototype.constructor = Deep;

Deep.prototype.toJs = function() {
  return this.name +".deep ? context.yes : context.no";
};

function Bump(name) {
  Expression.call(this);
  this.name = name;
}
Bump.prototype = Object.create(Expression.prototype);
Bump.prototype.constructor = Bump;

Bump.prototype.toJs = function() {
  return this.name + ".bump()";
};

function Same(one, two) {
  Expression.call(this);
  this.one = one;
  this.two = two;
}
Same.prototype = Object.create(Expression.prototype);
Same.prototype.constructor = Same;

Same.prototype.toJs = function() {
  return "(" + this.one + ".equals(" + this.two + ")" + " ? context.yes : context.no)";
};

function If(test, yes, no) {
  Statement.call(this);
  this.test = test;
  this.yes  = yes;
  this.no   = no;
}
If.prototype = Object.create(Statement.prototype);
If.prototype.constructor = If;

If.prototype.toJs = function() {
  return "if(" + this.test + ".loob()){" +
    this.yes.toJs() + "}else{" + this.no.toJs() + "}";
};

function Kick(axis, core, tail) {
  Expression.call(this)
  this.axis = axis;
  this.core = core;
  this.tail = tail;
}
Kick.prototype = Object.create(Expression.prototype);
Kick.prototype.constructor = Kick;

Kick.prototype.toJs = function() {
  var axis = this.axis.shortCode();

  return "(function (cor) {" +
           "var pro, tgt, bus, arms, bat = cor.head, has = false;" +
           "if ( bat.hasOwnProperty('loc') && (tgt = bat.loc.jets[" + axis + "]) && bat.loc.fine(cor) ) {" +
             "return tgt(cor);" +
           "}" +
           "if ( bat.hasOwnProperty('arms') ) {" +
             "arms = bat.arms;" +
             "has = arms.hasOwnProperty('" + axis + "');" + 
           "}" +
           "else arms = bat.arms = {};" +
           "tgt = (has ? arms['" + axis + "'] : (arms['" + axis + "'] = context.compile(" +
             new Frag(this.axis, "bat").toJs() + ")));" +
           "bus = cor;" +
           (this.tail ? "pro = context.trampoline(tgt, bus);" :
             "while (true) {" +
               "pro = tgt(bus);" +
               "if ( context.isTrampoline(pro) ) {" +
                 "tgt = pro.target;" +
                 "bus = pro.subject;" +
               "}" +
               "else break;" +
             "}") +
           "return pro;" +
         "})(" + this.core + ")";
};

function GetMemo(name) {
  Expression.call(this);
  this.name = name;
}
GetMemo.prototype = Object.create(Expression.prototype);
GetMemo.prototype.constructor = GetMemo;

GetMemo.prototype.toJs = function() {
  return "context.getMemo(" + this.name + ")";
};

function PutMemo(key, val) {
  Statement.call(this);
  this.key = key;
  this.val = val;
}
PutMemo.prototype = Object.create(Statement.prototype);
PutMemo.prototype.constructor = PutMemo;

function Push(name) {
  Statement.call(this);
  this.name = name;
}
Push.prototype = Object.create(Statement.prototype);
Push.prototype.constructor = Push;

Push.prototype.toJs = function() {
  return "context.stackPush(" + this.name + ");";
};

function Pop() {
  Statement.call(this);
}
Pop.prototype = Object.create(Statement.prototype);
Pop.prototype.constructor = Pop;

Pop.prototype.toJs = function() {
  return "context.stackPop()";
};

function Fast(clue, core) {
  Statement.call(this);
  this.clue = clue;
  this.core = core;
}

Fast.prototype.toJs = function() {
  return "context.register(" + this.core + ", " + this.clue + ");";
}

function Slog(name) {
  Statement.call(this);
  this.name = name;
}

Slog.prototype.toJs = function() {
  return "context.slog(" + this.name + ")";
};

function compile(formula, subject, product, fresh, constants, block, tail) {
  var op, arg, one, two, odd;
  if ( !(formula instanceof Cell )) {
    throw new Error("invalid formula");
  }
  op = formula.head;
  arg = formula.tail;
  if ( op instanceof Cell ) {
    one = fresh();
    two = fresh();
    compile(op, subject, one, fresh, constants, block, false);
    compile(arg, subject, two, fresh, constants, block, false);
    block.append(new Assignment(product, new Cons(one, two)));
  }
  else switch ( op.valueOf() ) {
    case 0:
      if ( 0 === arg ) {
        block.append(new Bail());
      }
      else if ( 1 === arg ) {
        block.append(new Identity(subject));
      }
      else {
        block.append(new Assignment(product, new Frag(arg, subject)));
      }
      break;
    case 1:
      constants.push(arg);
      block.append(new Assignment(product, new Constant(constants.length - 1)));
      break;
    case 2:
      one = fresh();
      two = fresh();
      compile(arg.head, subject, one, fresh, constants, block, false);
      compile(arg.tail, subject, two, fresh, constants, block, false);
      block.append(new Assignment(product, new Nock(one, two, tail)));
      break;
    case 3:
      one = fresh();
      compile(arg, subject, one, fresh, constants, block, false);
      block.append(new Assignment(product, new Deep(one)));
      break;
    case 4:
      one = fresh();
      compile(arg, subject, one, fresh, constants, block, false);
      block.append(new Assignment(product, new Bump(one)));
      break;
    case 5:
      one = fresh();
      two = fresh();
      compile(arg.head, subject, one, fresh, constants, block, false);
      compile(arg.tail, subject, two, fresh, constants, block, false);
      block.append(new Assignment(product, new Same(one, two)));
      break;
    case 6:
      odd = fresh();
      one = new Block();
      two = new Block();
      compile(arg.head, subject, odd, fresh, constants, block, false);
      compile(arg.tail.head, subject, product, fresh, constants, one, tail);
      compile(arg.tail.tail, subject, product, fresh, constants, two, tail);
      block.append(new If(odd, one, two));
      break;
    case 7:
      one = fresh();
      compile(arg.head, subject, one, fresh, constants, block, false);
      compile(arg.tail, one, product, fresh, constants, block, tail);
      break;
    case 8:
      one = fresh();
      two = fresh();
      compile(arg.head, subject, one, fresh, constants, block, false);
      block.append(new Assignment(two, new Cons(one, subject)));
      compile(arg.tail, two, product, fresh, constants, block, tail);
      break;
    case 9:
      odd = arg.head;
      if ( 2 === odd.cap().valueOf() ) {
        one = fresh();
        two = odd.mas();
        compile(arg.tail, subject, one, fresh, constants, block, false);
        block.append(new Assignment(product, new Kick(two, one, tail)));
      }
      else {
        compile(noun.dwim([7, arg.tail, 2, [0, 1], 0, odd]),
          subject, product, fresh, constants, block, tail);
      }
      break;
    case 10:
      var hint = arg.head;
      if ( !(arg.head instanceof Cell) ) {
        // no recognized static hints
        compile(arg.tail, subject, product, fresh, constants, block, tail);
      }
      else {
        var zep = hint.head;
        var clu = fresh();
        compile(hint.tail, subject, clu, fresh, constants, block, false);
        if ( zep.equals(MEMO) ) {
          var key = fresh();
          var got = fresh();
          odd = fresh();
          one = new Block();
          two = new Block();
          var konst = fresh();
          block.append(new Assignment(konst, new Constant(hint.tail)));
          block.append(new Assignment(key, new Cons(subject, konst)));
          block.append(new Assignment(got, new GetMemo(two)));
          block.append(new Assignment(odd, new Deep(got)));
          one.append(new Assignment(product, new Frag(noun.dwim(3), got)));
          compile(arg.tail, subject, product, fresh, two, false);
          two.append(new PutMemo(key, product));
          block.append(new If(odd, one, two));
        }
        else if ( zep.equals(SLOG) ) {
          block.append(new Slog(clu));
          compile(arg.tail, subject, product, fresh, constants, block, tail);
        }
        else if ( zep.equals(FAST) ) {
          compile(arg.tail, subject, product, fresh, constants, block, false);
          block.append(new Fast(clu, product));
        }
        else if ( zep.equals(SPOT) ||
                  zep.equals(MEAN) ||
                  zep.equals(HUNK) ||
                  zep.equals(LOSE) ) {
          one = fresh();
          two = fresh();
          block.append(new Assignment(one, new Constant(zep)));
          block.append(new Assignment(two, new Cons(one, clu)));
          block.append(new Push(two));
          compile(arg.tail, subject, product, fresh, constants, block, false);
          block.append(new Pop());
        }
        else {
          // unrecognized
          compile(arg.tail, subject, product, fresh, constants, block, tail);
        }
      }
      break;
    case 11:
      one = fresh();
      two = fresh();
      compile(arg.head, subject, one, fresh, constants, block, false);
      compile(arg.tail, subject, two, fresh, constants, block, false);
      block.append(new Assignment(product, new Esc(one, two)));
      break;
    default:
      throw new Error("invalid opcode");
  }
}

function Trampoline(target, subject) {
  this.target = target;
  this.subject = subject;
}

function genFine(loc) {
  var constants = [], out = [], i;
  for ( i = 0; !loc.isStatic; ++i ) {
    out.push("if(!constants[" + i + "].equals(a.head)){return false;}");
    constants.push(loc.noun);
    out.push("a=" + new Frag(loc.axisToParent, "a").toJs() + ";");
    loc = loc.parentLoc;
  }
  out.push("return constants[" + i + "].equals(a);");
  constants.push(loc.noun);
  var body = 'return function(a){' + out.join('') + 'return true;};';
  var builder = new Function('constants', body);
  return builder(constants);
}

var three = noun.dwim(3);
function Location(context, name, label, axisToParent, hooks, noun, parentLoc) {
  this.name = name;
  this.label = label;
  this.parentLoc = parentLoc;
  this.axisToParent = axisToParent;
  this.fragToParent = Noun.fragmenter(axisToParent);
  this.nameToAxis = hooks;
  this.axisToName = {};
  this.isStatic = ( null === parentLoc || (three.equals(axisToParent) && parentLoc.isStatic) );
  if ( this.isStatic ) {
    this.noun = noun;
  }
  else {
    this.noun = noun.head;
    this.noun.mug();
  }
  for ( var k in hooks ) {
    if ( hooks.hasOwnProperty(k) ) {
      this.axisToName[hooks[k].shortCode()] = k;
    }
  }
  this.jets = {};
  var drivers = context.drivers[label];
  if ( drivers && drivers.length > 0 ) {
    for ( var i = 0; i < drivers.length; ++i ) {
      var d = drivers[i];
      if ( d instanceof AxisArm ) {
        this.jets[d.axis.mas().shortCode()] = d.fn;
      }
      else {
        this.jets[nameToAxis[d.name].mas().shortCode()] = d.fn;
      }
    }
  }
  this.fine = genFine(this);
}

function Clue(name, parentAxis, hooks) {
  this.name = name;
  this.parentAxis = parentAxis;
  this.hooks = hooks;
}

function JetDriver(label, fn) {
  this.label = label;
  this.fn = fn;
}

function AxisArm(label, axis, fn) {
  JetDriver.call(this, label, fn);
  this.axis = axis;
}
AxisArm.prototype = Object.create(JetDriver.prototype);
AxisArm.prototype.constructor = AxisArm;

function NamedArm(label, name, fn) {
  JetDriver.call(this, label, fn);
  this.name = name;
}
NamedArm.prototype = Object.create(JetDriver.prototype);
NamedArm.prototype.constructor = NamedArm;

var two = noun.dwim(2);
function collectFromCore(prefix, spec, out) {
  var name = spec[0], arms = spec[1], children = spec[2],
      labl = prefix + "/" + name;
  if ( arms instanceof Function ) {
    out[labl] = [new AxisArm(labl, two, arms)];
  }
  else {
    var all = [];
    for ( var k in arms ) {
      if ( arms.hasOwnProperty(k) ) {
        all.push(( 'number' === typeof(k) ) ?
          new AxisArm(labl, noun.dwim(k), arms[k]) :
          new NamedArm(labl, k, arms[k]));
      }
    }
    out[labl] = all;
  }
  if ( children ) {
    for ( var i = 0; i < children.length; ++i ) {
      collectFromCore(labl, children[i], out);
    }
  }
}

function Context(drivers) {
  this.memo    = new NounMap();
  this.clues   = new NounMap();
  this.dash    = new NounMap();
  this.tax     = noun.Atom.yes;
  this.drivers = {};
  if ( drivers ) {
    collectFromCore('', drivers, this.drivers);
  }
}

Context.prototype.yes = noun.Atom.yes;
Context.prototype.no = noun.Atom.no;
Context.prototype.cons = function (h, t) {
  return new Cell(h, t);
};

Context.prototype.trampoline = function(tgt, bus) {
  return new Trampoline(tgt, bus);
};

Context.prototype.isTrampoline = function(a) {
  return (a instanceof Trampoline);
};

Context.prototype.compile = function(cell) {
  var i = 0;
  var fresh = function() {
    return "v" + ++i;
  };
  var body = new Block();
  var constants = [];
  compile(cell, "subject", "product", fresh, constants, body, true);
  var text = "return function(subject){" + body.toJs() + "return product;}";
  var builder = new Function("context", "constants", text);
  return cell.target = builder(this, constants);
};

Context.prototype.nock = function(subject, formula) {
  var product, target;
  if ( !formula.hasOwnProperty("target") ) {
    this.compile(formula);
  }
  target = formula.target;
  while ( true ) {
    product = target(subject);
    if ( product instanceof Trampoline ) {
      subject = product.subject;
      target  = product.target;
    }
    else {
      return product;
    }
  }
};

Context.prototype.getMemo = function(key) {
  return this.memo.get(key);
}

Context.prototype.putMemo = function(key, val) {
  this.memo.insert(key, val);
};

Context.prototype.stackPush = function(item) {
  this.tax = new Cell(item, this.tax);
};

Context.prototype.stackPop = function() {
  this.tax = this.tax.tail;
}

Context.prototype.slog = function(item) {
  // TODO: don't rewrite ++wash again, just call the kernel
  console.log(item);
};

function chum(n) {
  if ( n.deep ) {
    return Atom.cordToString(n.head) + n.tail.number.shortValue().toString(10);
  }
  else {
    return Atom.cordToString(n);
  }
}

var ten = noun.dwim(10);
function skipHints(formula) {
  while ( true ) {
    if ( formula.deep ) {
      if ( ten.equals(formula.head) ) {
        formula = formula.tail.tail;
        continue;
      }
    }
    return formula;
  }
}

var zero = noun.dwim(0), constant_zero = noun.dwim(1,0);
function parseParentAxis(noun) {
  var f = skipHints(noun);
  if ( constant_zero.equals(f) ) {
    return zero;
  }
  else if ( !zero.equals(f.head) ) {
    throw new Error("weird formula head");
  }
  else if ( 3 != f.tail.cap().valueOf() ) {
    throw new Error("weird parent axis");
  }
  return f.tail;
}

var nine = noun.dwim(9), constant_frag = noun.dwim(0,1);
function parseHookAxis(nock) {
  var f = skipHints(nock),
     op = f.head;
  if ( !op.deep ) {
    if ( zero.equals(op) ) {
      if ( !f.tail.deep ) {
        return f.tail;
      }
    }
    else if ( nine.equals(op) ) {
      var rest = f.tail;
      if ( !rest.head.deep && constant_frag.equals(rest.tail) ) {
        return rest.head;
      }
    }
  }
  return null;
}

function parseHooks(noun) {
  var o = {};
  list.forEach(noun, function(c) {
    var term = Atom.cordToString(c.head),
        axis = parseHookAxis(c.tail);
    if ( null != axis ) {
      o[term] = axis;
    }
  });
  return o;
}

Context.prototype.parseClue = function(raw) {
  var clue = this.clues.get(raw);
  if ( clue === undefined ) {
    var name = chum(raw.head),
        parentAxis = parseParentAxis(raw.tail.head),
        hooks = parseHooks(raw.tail.tail);
    clue = new Clue(name, parentAxis, hooks);
    this.clues.insert(raw, clue);
  }
  return clue;
}

Context.prototype.register = function(core, raw) {
  var bat = core.head;
  var loc = this.dash.get(bat);
  if ( undefined === loc ) {
    try {
      var clue = this.parseClue(raw);
      if ( zero.equals(clue.parentAxis) ) {
        loc = new Location(this, clue.name, '/' + clue.name, zero, clue.hooks, core, null);
      }
      else {
        var parentCore    = core.at(clue.parentAxis),
            parentBattery = parentCore.head,
            parentLoc     = this.dash.get(parentBattery);
        if ( undefined === parentLoc ) {
          console.log('register: invalid parent for ' + clue.name);
        }
        else {
          var label = parentLoc.label + "/" + clue.name;
          loc = new Location(this, clue.name, label, clue.parentAxis, clue.hooks, core, parentLoc);
        }
      }
      bat.loc = loc;
      this.dash.insert(bat, loc);
    }
    catch (e) {
      console.log(e);
    }
  }
};

module.exports = {
  Context: Context,
}
