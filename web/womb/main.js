(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Dispatcher, Persistence;

Dispatcher = require('./Dispatcher.coffee');

Persistence = require('./Persistence.coffee');

module.exports = {
  getData: function(path) {
    return Persistence.get(path, function(err, arg) {
      var data;
      data = arg.data;
      if (err != null) {
        throw new "Server error";
      }
      return Dispatcher.dispatch({
        gotData: {
          path: path,
          data: data
        }
      });
    });
  }
};


},{"./Dispatcher.coffee":2,"./Persistence.coffee":3}],2:[function(require,module,exports){
module.exports = new Flux.Dispatcher();


},{}],3:[function(require,module,exports){
module.exports = {
  get: function(path, cb) {
    return urb.bind("/scry/x/womb" + path, {
      appl: "hood"
    }, cb);
  }
};


},{}],4:[function(require,module,exports){
var EventEmitter, WombDispatcher, WombStore, _data, unpackFrond;

EventEmitter = require('events').EventEmitter;

unpackFrond = require('./util.coffee').unpackFrond;

WombDispatcher = require('./Dispatcher.coffee');

_data = {};

WombStore = _.extend(new EventEmitter, {
  emitChange: function() {
    return this.emit('change');
  },
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  retrieve: function(path) {
    return _data[path];
  },
  gotData: function(arg1) {
    var data, path;
    path = arg1.path, data = arg1.data;
    return _data[path] = data;
  }
});

WombStore.dispatchToken = WombDispatcher.register(function(action) {
  var arg, ref, type;
  ref = unpackFrond(action), type = ref[0], arg = ref[1];
  if (WombStore[type]) {
    WombStore[type](arg);
    return WombStore.emitChange();
  }
});

module.exports = WombStore;


},{"./Dispatcher.coffee":2,"./util.coffee":9,"events":10}],5:[function(require,module,exports){
var Actions, Store, div, i, recl, ref, rele;

Actions = require('../Actions.coffee');

Store = require('../Store.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, i = ref.i;

module.exports = function(path, Child) {
  return recl({
    displayName: "Async",
    getInitialState: function() {
      return this.retrieveData();
    },
    retrieveData: function() {
      return {
        data: Store.retrieve(path)
      };
    },
    componentDidMount: function() {
      Store.addChangeListener(this.changeListener);
      if (this.state.data == null) {
        return Actions.getData(path);
      }
    },
    componentWillUnmount: function() {
      return Store.removeChangeListener(this.changeListener);
    },
    changeListener: function() {
      if (this.isMounted()) {
        return this.setState(this.retrieveData());
      }
    },
    render: function() {
      return div({}, this.state.data == null ? i({
        key: "load"
      }, "Fetching data...") : rele(Child, _.extend({}, this.props, {
        key: "got",
        data: this.state.data
      }), this.props.children));
    }
  });
};


},{"../Actions.coffee":1,"../Store.coffee":4}],6:[function(require,module,exports){
var Ships, div, rele;

Ships = require('./Ships.coffee');

rele = React.createElement;

div = React.DOM.div;

module.exports = function() {
  return div({}, rele(Ships, {}));
};


},{"./Ships.coffee":7}],7:[function(require,module,exports){
var Async, code, div, p, pre, ref;

Async = require('./Async.coffee');

ref = React.DOM, p = ref.p, div = ref.div, pre = ref.pre, code = ref.code;

module.exports = Async("/stats", function(arg) {
  var data;
  data = arg.data;
  return div({}, p({}, "Womb stub: ships"), pre({}, code({}, JSON.stringify(data))));
});


},{"./Async.coffee":5}],8:[function(require,module,exports){
var MainComponent, TreeActions;

MainComponent = require('./components/Main.coffee');

TreeActions = window.tree.actions;

TreeActions.registerComponent("womb", MainComponent);


},{"./components/Main.coffee":6}],9:[function(require,module,exports){
var slice = [].slice;

module.exports = {
  unpackFrond: function(a) {
    var alts, key, ref;
    ref = _.keys(a), key = ref[0], alts = 2 <= ref.length ? slice.call(ref, 1) : [];
    if (!_.isEmpty(alts)) {
      throw new Error("Improper frond: " + ([key].concat(slice.call(alts)).join(',')));
    }
    return [key, a[key]];
  }
};


},{}],10:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

function EventEmitter() {
  this._events = this._events || {};
  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
EventEmitter.defaultMaxListeners = 10;

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function(n) {
  if (!isNumber(n) || n < 0 || isNaN(n))
    throw TypeError('n must be a positive number');
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.emit = function(type) {
  var er, handler, len, args, i, listeners;

  if (!this._events)
    this._events = {};

  // If there is no 'error' event listener then throw.
  if (type === 'error') {
    if (!this._events.error ||
        (isObject(this._events.error) && !this._events.error.length)) {
      er = arguments[1];
      if (er instanceof Error) {
        throw er; // Unhandled 'error' event
      }
      throw TypeError('Uncaught, unspecified "error" event.');
    }
  }

  handler = this._events[type];

  if (isUndefined(handler))
    return false;

  if (isFunction(handler)) {
    switch (arguments.length) {
      // fast cases
      case 1:
        handler.call(this);
        break;
      case 2:
        handler.call(this, arguments[1]);
        break;
      case 3:
        handler.call(this, arguments[1], arguments[2]);
        break;
      // slower
      default:
        args = Array.prototype.slice.call(arguments, 1);
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    args = Array.prototype.slice.call(arguments, 1);
    listeners = handler.slice();
    len = listeners.length;
    for (i = 0; i < len; i++)
      listeners[i].apply(this, args);
  }

  return true;
};

EventEmitter.prototype.addListener = function(type, listener) {
  var m;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events)
    this._events = {};

  // To avoid recursion in the case that type === "newListener"! Before
  // adding it to the listeners, first emit "newListener".
  if (this._events.newListener)
    this.emit('newListener', type,
              isFunction(listener.listener) ?
              listener.listener : listener);

  if (!this._events[type])
    // Optimize the case of one listener. Don't need the extra array object.
    this._events[type] = listener;
  else if (isObject(this._events[type]))
    // If we've already got an array, just append.
    this._events[type].push(listener);
  else
    // Adding the second element, need to change to array.
    this._events[type] = [this._events[type], listener];

  // Check for listener leak
  if (isObject(this._events[type]) && !this._events[type].warned) {
    if (!isUndefined(this._maxListeners)) {
      m = this._maxListeners;
    } else {
      m = EventEmitter.defaultMaxListeners;
    }

    if (m && m > 0 && this._events[type].length > m) {
      this._events[type].warned = true;
      console.error('(node) warning: possible EventEmitter memory ' +
                    'leak detected. %d listeners added. ' +
                    'Use emitter.setMaxListeners() to increase limit.',
                    this._events[type].length);
      if (typeof console.trace === 'function') {
        // not supported in IE 10
        console.trace();
      }
    }
  }

  return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.once = function(type, listener) {
  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  var fired = false;

  function g() {
    this.removeListener(type, g);

    if (!fired) {
      fired = true;
      listener.apply(this, arguments);
    }
  }

  g.listener = listener;
  this.on(type, g);

  return this;
};

// emits a 'removeListener' event iff the listener was removed
EventEmitter.prototype.removeListener = function(type, listener) {
  var list, position, length, i;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events || !this._events[type])
    return this;

  list = this._events[type];
  length = list.length;
  position = -1;

  if (list === listener ||
      (isFunction(list.listener) && list.listener === listener)) {
    delete this._events[type];
    if (this._events.removeListener)
      this.emit('removeListener', type, listener);

  } else if (isObject(list)) {
    for (i = length; i-- > 0;) {
      if (list[i] === listener ||
          (list[i].listener && list[i].listener === listener)) {
        position = i;
        break;
      }
    }

    if (position < 0)
      return this;

    if (list.length === 1) {
      list.length = 0;
      delete this._events[type];
    } else {
      list.splice(position, 1);
    }

    if (this._events.removeListener)
      this.emit('removeListener', type, listener);
  }

  return this;
};

EventEmitter.prototype.removeAllListeners = function(type) {
  var key, listeners;

  if (!this._events)
    return this;

  // not listening for removeListener, no need to emit
  if (!this._events.removeListener) {
    if (arguments.length === 0)
      this._events = {};
    else if (this._events[type])
      delete this._events[type];
    return this;
  }

  // emit removeListener for all listeners on all events
  if (arguments.length === 0) {
    for (key in this._events) {
      if (key === 'removeListener') continue;
      this.removeAllListeners(key);
    }
    this.removeAllListeners('removeListener');
    this._events = {};
    return this;
  }

  listeners = this._events[type];

  if (isFunction(listeners)) {
    this.removeListener(type, listeners);
  } else if (listeners) {
    // LIFO order
    while (listeners.length)
      this.removeListener(type, listeners[listeners.length - 1]);
  }
  delete this._events[type];

  return this;
};

EventEmitter.prototype.listeners = function(type) {
  var ret;
  if (!this._events || !this._events[type])
    ret = [];
  else if (isFunction(this._events[type]))
    ret = [this._events[type]];
  else
    ret = this._events[type].slice();
  return ret;
};

EventEmitter.prototype.listenerCount = function(type) {
  if (this._events) {
    var evlistener = this._events[type];

    if (isFunction(evlistener))
      return 1;
    else if (evlistener)
      return evlistener.length;
  }
  return 0;
};

EventEmitter.listenerCount = function(emitter, type) {
  return emitter.listenerCount(type);
};

function isFunction(arg) {
  return typeof arg === 'function';
}

function isNumber(arg) {
  return typeof arg === 'number';
}

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}

function isUndefined(arg) {
  return arg === void 0;
}

},{}]},{},[8]);
