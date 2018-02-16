(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Dispatcher, Persistence;

Dispatcher = require('./Dispatcher.coffee');

Persistence = require('./Persistence.coffee');

module.exports = {
  setPasscode: function(pass) {
    return Dispatcher.dispatch({
      setPasscode: pass
    });
  },
  recycleTicket: function(arg, pass) {
    var mail, ship, tick;
    ship = arg.ship, tick = arg.tick, mail = arg.mail;
    return Persistence.put("womb-recycle", {
      who: mail,
      him: "~" + ship,
      tik: "~" + tick
    }, (function(_this) {
      return function(err, arg1) {
        var data, status;
        status = arg1.status, data = arg1.data;
        if (status !== 200) {
          throw new Error("Server error: " + (JSON.stringify(data)));
        }
        _this.getData("/ticket/~" + ship + "/~" + tick, true);
        return _this.setPasscode(pass);
      };
    })(this));
  },
  confirmShip: function(pass, ship) {
    return Dispatcher.dispatch({
      confirmClaim: {
        pass: pass,
        ship: ship
      }
    });
  },
  claimShip: function(pass, ship) {
    Dispatcher.dispatch({
      putClaim: {
        pass: pass,
        ship: ship
      }
    });
    return Persistence.put("womb-claim", {
      aut: pass,
      her: ship
    }, (function(_this) {
      return function(err, arg) {
        var data, gotClaim, status;
        data = arg.data, status = arg.status;
        gotClaim = {
          pass: pass,
          ship: ship,
          own: true
        };
        if (status !== 200) {
          gotClaim.own = false;
        }
        Dispatcher.dispatch({
          gotClaim: gotClaim
        });
        _this.getData("/stats", true);
        return _this.getData("/balance/" + pass, true);
      };
    })(this));
  },
  getData: function(path, fresh) {
    if (fresh == null) {
      fresh = false;
    }
    return Persistence.get({
      path: path,
      fresh: fresh
    }, function(err, arg) {
      var data, status;
      status = arg.status, data = arg.data;
      if (err != null) {
        throw new Error("Client error");
      } else {
        return Dispatcher.dispatch({
          gotData: {
            path: path,
            data: data
          }
        });
      }
    });
  }
};


},{"./Dispatcher.coffee":2,"./Persistence.coffee":3}],2:[function(require,module,exports){
module.exports = new Flux.Dispatcher();


},{}],3:[function(require,module,exports){
var dup;

dup = {};

module.exports = {
  put: function(mark, data, cb) {
    return urb.send(data, {
      mark: mark,
      appl: "hood",
      wall: false
    }, cb);
  },
  get: function(arg, cb) {
    var fresh, path;
    path = arg.path, fresh = arg.fresh;
    if (!(dup[path] === "pending" || (!fresh && dup[path] === "got"))) {
      dup[path] = "pending";
      return urb.bind("/scry/x/womb" + path, {
        appl: "hood"
      }, function(err, dat) {
        cb(err, dat);
        return urb.drop("/scry/x/womb" + path, {
          appl: "hood"
        }, function() {
          return dup[path] = "got";
        });
      }, function(err, nice) {
        var ref;
        if (!(nice != null ? (ref = nice.data) != null ? ref.ok : void 0 : void 0)) {
          dup[path] = "got";
          return cb(err, nice);
        }
      });
    }
  }
};


},{}],4:[function(require,module,exports){
var EventEmitter, WombDispatcher, WombStore, _data, _default, unpackFrond;

EventEmitter = require('events').EventEmitter;

unpackFrond = require('./util.coffee').unpackFrond;

WombDispatcher = require('./Dispatcher.coffee');

_data = {
  pass: sessionStorage.womb_pass
};

_default = {
  claim: "none",
  pass: ""
};

WombStore = _.extend((new EventEmitter).setMaxListeners(50), {
  emitChange: function() {
    return this.emit('change');
  },
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  removeChangeListener: function(cb) {
    return this.removeListener("change", cb);
  },
  retrieve: function(path) {
    var ref;
    return (ref = _data[path]) != null ? ref : _default[path.split("/")[0]];
  },
  gotData: function(arg1) {
    var data, path;
    path = arg1.path, data = arg1.data;
    return _data[path] = data;
  },
  confirmClaim: function(arg1) {
    var k, ship, v;
    ship = arg1.ship;
    for (k in _data) {
      v = _data[k];
      if (k.indexOf('claim/') !== -1 && v === "confirm") {
        _data[k] = "none";
      }
    }
    return _data["claim/" + ship] = "confirm";
  },
  putClaim: function(arg1) {
    var ship;
    ship = arg1.ship;
    return _data["claim/" + ship] = "wait";
  },
  gotClaim: function(arg1) {
    var own, ship;
    ship = arg1.ship, own = arg1.own;
    return _data["claim/" + ship] = (own ? "own" : "xeno");
  },
  setPasscode: function(pass) {
    _data.pass = pass;
    return sessionStorage.womb_pass = pass != null ? pass : "";
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


},{"./Dispatcher.coffee":2,"./util.coffee":18,"events":20}],5:[function(require,module,exports){
var Actions, Balance, FromStore, History, InfoBox, Label, Mail, PassInput, Planets, Recycling, SHOP, Scry, Shop, Stars, b, clas, code, div, h3, h6, name, p, recl, ref, rele, span;

clas = require('classnames');

Actions = require('../Actions.coffee');

FromStore = (Scry = require('./Scry.coffee')).FromStore;

Label = require('./Label.coffee');

InfoBox = require('./InfoBox.coffee');

Shop = require('./Shop.coffee');

PassInput = require('./PassInput.coffee');

Recycling = require('./Recycling.coffee');

recl = React.createClass;

rele = React.createElement;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

ref = React.DOM, div = ref.div, b = ref.b, h3 = ref.h3, h6 = ref.h6, p = ref.p, span = ref.span, code = ref.code;

SHOP = true;

if (!SHOP) {
  Shop = function(type, length) {
    return function(arg) {
      arg;
      return h6({}, "Distribution of ", type, " not yet live.");
    };
  };
}

Mail = function(email) {
  return code({
    className: "email"
  }, email);
};

History = function(history) {
  var key, who;
  if (!history.length) {
    return "purchased directly from Tlon Inc. ";
  } else {
    return span({}, "previously owned by ", (function() {
      var i, len, results;
      results = [];
      for (key = i = 0, len = history.length; i < len; key = ++i) {
        who = history[key];
        results.push(span({
          key: key
        }, Mail(who)));
      }
      return results;
    })(), "and Tlon Inc. ");
  }
};

Stars = Shop("stars", 7);

Planets = Shop("planets", 14);

Balance = Scry("/balance/:pass", function(arg) {
  var balance, history, owner, planets, stars;
  balance = arg.balance;
  if (balance.fail) {
    return div({
      style: {
        marginTop: '1rem'
      }
    }, Label("Invalid passcode", "warning"));
  }
  planets = balance.planets, stars = balance.stars, owner = balance.owner, history = balance.history;
  return div({}, h3({}, "Balance"), p({}, "Hello ", Mail(owner), ", "), p({}, "This balance was ", History(history)), p({}, "You currently hold ", b({}, planets || "no"), " Planets ", "and ", b({}, stars || "no"), " Stars."), p({
    className: 'red'
  }, b({}, "Warning: "), "When you click 'Claim' we will send the ticket to the email address above.  This can only be done once!"), stars ? rele(Stars) : void 0, planets ? rele(Planets) : void 0);
});

module.exports = name("Claim", FromStore("pass", function(arg) {
  var pass;
  pass = arg.pass;
  return div({}, p({}, "To view your ships, input your passcode."), PassInput({
    minLength: 28,
    defaultValue: pass,
    onInputPass: Actions.setPasscode
  }), pass ? rele(Balance, {
    pass: pass
  }) : div({}, h3({}, "Convert an old ticket"), rele(Recycling, {})));
}));


},{"../Actions.coffee":1,"./InfoBox.coffee":7,"./Label.coffee":8,"./PassInput.coffee":11,"./Recycling.coffee":12,"./Scry.coffee":13,"./Shop.coffee":16,"classnames":19}],6:[function(require,module,exports){
var Actions, ClaimButton, FromStore, Label, ShipInput, _ClaimButton, button, name, recl, rele;

Actions = require('../Actions.coffee');

FromStore = require('./Scry.coffee').FromStore;

Label = require('./Label.coffee');

ShipInput = require('./ShipInput.coffee');

button = React.DOM.button;

recl = React.createClass;

rele = React.createElement;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

ClaimButton = FromStore("pass", function(arg) {
  var pass, ship;
  pass = arg.pass, ship = arg.ship;
  if (!ship) {
    return button({
      disabled: true,
      className: 'claim invalid'
    }, "Invalid");
  }
  return rele(_ClaimButton, {
    pass: pass,
    ship: ship
  });
});

_ClaimButton = FromStore("claim/:ship", function(arg) {
  var claim, pass, ship;
  claim = arg.claim, pass = arg.pass, ship = arg.ship;
  switch (claim) {
    case "own":
      return Label("Claimed!", "success");
    case "wait":
      return Label("Claiming...");
    case "xeno":
      return Label("Not available", "warning");
    case "none":
      return button({
        onClick: function() {
          return Actions.confirmShip(pass, ship);
        }
      }, "Claim");
    case "confirm":
      return button({
        onClick: function() {
          return Actions.claimShip(pass, ship);
        }
      }, "Click again to confirm.");
    default:
      throw new Error("Bad claim type: " + claim);
  }
});

module.exports = name("ClaimButton", ClaimButton);


},{"../Actions.coffee":1,"./Label.coffee":8,"./Scry.coffee":13,"./ShipInput.coffee":14}],7:[function(require,module,exports){
var a, div, recl, ref,
  slice = [].slice;

ref = React.DOM, a = ref.a, div = ref.div;

recl = React.createClass;

module.exports = recl({
  displayName: "InfoBox",
  getInitialState: function() {
    return {
      expanded: false
    };
  },
  onClick: function() {
    return this.setState({
      expanded: !this.state.expanded
    });
  },
  render: function() {
    var contents, expanded, prompt, ref1;
    expanded = this.state.expanded;
    ref1 = this.props.children, prompt = ref1[0], contents = 2 <= ref1.length ? slice.call(ref1, 1) : [];
    return div({
      className: "info"
    }, a({
      onClick: this.onClick
    }, prompt), expanded ? div.apply(null, [{
      "info contents": "info contents"
    }].concat(slice.call(contents))) : void 0);
  }
});


},{}],8:[function(require,module,exports){
var span;

span = React.DOM.span;

module.exports = function(s, type) {
  if (type == null) {
    type = "default";
  }
  return span({
    className: "label label-" + type
  }, s);
};


},{}],9:[function(require,module,exports){
var input, mailShape, name, recl;

mailShape = require('../util.coffee').mailShape;

input = React.DOM.input;

recl = React.createClass;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

module.exports = name("MailInput", function(arg) {
  var onInputMail;
  onInputMail = arg.onInputMail;
  return input({
    placeholder: "me@example.com",
    onChange: function(arg1) {
      var mail, target;
      target = arg1.target;
      mail = target.value.trim();
      return onInputMail((mailShape(mail) ? mail : void 0));
    }
  });
});


},{"../util.coffee":18}],10:[function(require,module,exports){
var Claim, NET, Recycling, Ships, a, div, h3, h4, ref, rele;

Claim = require('./Claim.coffee');

Ships = require('./Ships.coffee');

Recycling = require('./Recycling.coffee');

rele = React.createElement;

NET = true;

ref = React.DOM, div = ref.div, h3 = ref.h3, h4 = ref.h4, a = ref.a;

module.exports = function() {
  return div({}, h3({
    className: 'first-a'
  }, "Claim an invite"), rele(Claim, {}), NET ? div({}, h3({}, "Network"), rele(Ships, {})) : void 0, div({
    className: 'footer'
  }, "Questions?  Email us:", a({
    href: "mailto:urbit@urbit.org"
  }, "urbit@urbit.org"), "."));
};


},{"./Claim.coffee":5,"./Recycling.coffee":12,"./Ships.coffee":15}],11:[function(require,module,exports){
var input, name, recl, uvShape;

uvShape = require('../util.coffee').uvShape;

input = React.DOM.input;

recl = React.createClass;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

module.exports = name("PassInput", function(arg) {
  var defaultValue, minLength, onInputPass;
  onInputPass = arg.onInputPass, minLength = arg.minLength, defaultValue = arg.defaultValue;
  return input({
    defaultValue: defaultValue,
    className: 'mono',
    style: {
      width: '100%'
    },
    placeholder: "0v0.00000.00000.00000.00000.00000",
    onChange: function(arg1) {
      var pass, target;
      target = arg1.target;
      pass = target.value.trim();
      return onInputPass(((uvShape(pass)) && pass.length >= minLength ? pass : void 0));
    }
  });
});


},{"../util.coffee":18}],12:[function(require,module,exports){
var Actions, Label, MailInput, RecycleButton, RecycleTicket, Recycling, Scry, ShipInput, a, button, div, name, recl, ref, rele, span;

Actions = require('../Actions.coffee');

Scry = require('./Scry.coffee');

Label = require('./Label.coffee');

ShipInput = require('./ShipInput.coffee');

MailInput = require('./MailInput.coffee');

ref = React.DOM, a = ref.a, div = ref.div, span = ref.span, button = ref.button;

recl = React.createClass;

rele = React.createElement;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

RecycleButton = name("RecycleButton", function(arg) {
  var disabled, onClick;
  disabled = arg.disabled, onClick = arg.onClick;
  if (!disabled) {
    return button({
      onClick: onClick
    }, "Exchange");
  } else {
    return button({
      disabled: disabled
    }, "Exchange (email required)");
  }
});

RecycleTicket = name("RecycleTicket", Scry("/ticket/~:ship/~:tick", function(arg) {
  var doRecycle, mail, passcode, ref1, ship, status, tick;
  ship = arg.ship, tick = arg.tick, mail = arg.mail, (ref1 = arg.ticket, passcode = ref1.passcode, status = ref1.status);
  doRecycle = function() {
    return Actions.recycleTicket({
      ship: ship,
      tick: tick,
      mail: mail
    }, passcode);
  };
  return div({
    className: 'recycleTicket'
  }, (function() {
    switch (status != null ? status : "fail") {
      case "fail":
        return Label("Bad ticket", "warning");
      case "good":
        return rele(RecycleButton, {
          disabled: !mail,
          onClick: doRecycle
        });
      case "used":
        return span({}, a({
          onClick: function() {
            return Actions.setPasscode(passcode);
          }
        }, passcode), Label("Ticket exchanged", "info"));
      default:
        throw new Error("Bad ticket status: " + status);
    }
  })());
}));

Recycling = recl({
  getInitialState: function() {
    return {
      ship: "",
      tick: "",
      mail: ""
    };
  },
  render: function() {
    var getMail, getShip, getTick, mail, ref1, ship, tick;
    getShip = rele(ShipInput, {
      placeholder: 'some-ship',
      length: 14,
      oldFormat: true,
      onInputShip: (function(_this) {
        return function(ship) {
          return _this.setState({
            ship: ship
          });
        };
      })(this)
    });
    getTick = rele(ShipInput, {
      placeholder: 'some-sample-ticket-code',
      length: 28,
      oldFormat: true,
      onInputShip: (function(_this) {
        return function(tick) {
          return _this.setState({
            tick: tick
          });
        };
      })(this)
    });
    getMail = rele(MailInput, {
      onInputMail: (function(_this) {
        return function(mail) {
          return _this.setState({
            mail: mail
          });
        };
      })(this)
    });
    ref1 = this.state, ship = ref1.ship, tick = ref1.tick, mail = ref1.mail;
    return div({
      className: "recycling"
    }, "To convert an old ship and ticket, input your information here.", div({}, div({
      className: 'label'
    }, "Planet"), getShip, (ship ? Label("✓", "success") : void 0)), div({}, div({
      className: 'label'
    }, "Ticket"), getTick, (tick ? Label("✓", "success") : void 0)), div({}, div({
      className: 'label'
    }, "Email"), getMail, (mail ? Label("✓", "success") : void 0)), ship && tick ? rele(RecycleTicket, {
      ship: ship,
      tick: tick,
      mail: mail
    }) : void 0);
  }
});

module.exports = name("Recycling", Recycling);


},{"../Actions.coffee":1,"./Label.coffee":8,"./MailInput.coffee":9,"./Scry.coffee":13,"./ShipInput.coffee":14}],13:[function(require,module,exports){
var Actions, FromStore, Scry, Store, div, i, recl, ref, rele;

Actions = require('../Actions.coffee');

Store = require('../Store.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, i = ref.i;

FromStore = function(path, Child) {
  return recl({
    displayName: "FromStore." + path.split('/').join('-'),
    getInitialState: function() {
      return this.retrieveData();
    },
    retrieveData: function() {
      var data, obj;
      data = Store.retrieve(this.getPath());
      return (
        obj = {
          loaded: data != null
        },
        obj["" + (this.getKey())] = data,
        obj
      );
    },
    getKey: function() {
      return path.match(/[a-z0-9-]+/)[0];
    },
    getPath: function() {
      return path.replace(/:([a-z0-9_.~-]+)/g, (function(_this) {
        return function(m, key) {
          return _this.props[key];
        };
      })(this));
    },
    componentDidMount: function() {
      return Store.addChangeListener(this.changeListener);
    },
    componentWillUnmount: function() {
      return Store.removeChangeListener(this.changeListener);
    },
    componentDidUpdate: function(_props, _state) {
      if (_props !== this.props) {
        return this.setState(this.retrieveData());
      }
    },
    changeListener: function() {
      if (this.isMounted()) {
        return this.setState(this.retrieveData());
      }
    },
    render: function() {
      return rele(Child, _.extend({}, this.props, this.state, {
        path: this.getPath()
      }));
    }
  });
};

Scry = function(path, Child) {
  return FromStore(path, recl({
    displayName: "Scry",
    checkProps: function() {
      if (!this.props.loaded) {
        return Actions.getData(this.props.path);
      }
    },
    componentDidMount: function() {
      return this.checkProps();
    },
    componentDidUpdate: function(_props, _state) {
      return this.checkProps();
    },
    render: function() {
      return div({
        style: {
          display: "inline"
        }
      }, !this.props.loaded ? i({
        key: "load",
        style: {
          marginTop: '1rem',
          display: 'block'
        }
      }, "Loading...") : rele(Child, _.extend({}, this.props, {
        key: "got"
      }), this.props.children));
    }
  }));
};

module.exports = Scry;

module.exports.FromStore = FromStore;


},{"../Actions.coffee":1,"../Store.coffee":4}],14:[function(require,module,exports){
var input, name, recl, shipShape;

shipShape = require('../util.coffee').shipShape;

input = React.DOM.input;

recl = React.createClass;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

module.exports = name("ShipInput", function(arg) {
  var defaultValue, length, oldFormat, onInputShip, placeholder;
  onInputShip = arg.onInputShip, length = arg.length, defaultValue = arg.defaultValue, oldFormat = arg.oldFormat, placeholder = arg.placeholder;
  return input({
    defaultValue: defaultValue,
    placeholder: placeholder,
    className: 'mono pick',
    onChange: function(arg1) {
      var ship, target;
      target = arg1.target;
      ship = target.value.trim();
      if (ship[0] !== '~') {
        ship = "~" + ship;
      }
      return onInputShip(((shipShape(ship, oldFormat)) && ship.length === length ? ship.slice(1) : void 0));
    }
  });
});


},{"../util.coffee":18}],15:[function(require,module,exports){
var Label, Scry, Stat, clas, code, div, labels, li, name, p, pre, recl, ref, rele, span, ul;

clas = require('classnames');

Scry = require('./Scry.coffee');

Label = require('./Label.coffee');

recl = React.createClass;

rele = React.createElement;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

ref = React.DOM, p = ref.p, ul = ref.ul, li = ref.li, span = ref.span, div = ref.div, pre = ref.pre, code = ref.code;

labels = {
  free: "Unallocated",
  owned: "Issued",
  split: "Distributing"
};

Stat = name("Stat", function(arg) {
  var className, dist, free, live, owned, ship, split, stats;
  stats = arg.stats;
  return ul({
    className: 'network'
  }, (function() {
    var ref1, results;
    results = [];
    for (ship in stats) {
      ref1 = stats[ship], live = ref1.live, dist = ref1.dist;
      free = dist.free, owned = dist.owned, split = dist.split;
      className = clas(dist);
      results.push(li({
        className: className,
        key: ship
      }, span({
        className: "mono"
      }, "~" + ship), (function() {
        switch (false) {
          case free == null:
            return Label(labels.free);
          case owned == null:
            return Label(labels.owned);
          case split == null:
            if (_.isEmpty(split)) {
              return Label(labels.split);
            } else {
              return rele(Stat, {
                stats: split
              });
            }
            break;
          default:
            throw new Error("Bad stat: " + (_.keys(dist)));
        }
      })()));
    }
    return results;
  })());
});

module.exports = Scry("/stats", Stat);


},{"./Label.coffee":8,"./Scry.coffee":13,"classnames":19}],16:[function(require,module,exports){
var ClaimButton, Scry, ShipInput, Shop, ShopShips, button, code, div, h6, li, recl, ref, rele, span, ul;

Scry = require('./Scry.coffee');

ShipInput = require('./ShipInput.coffee');

ClaimButton = require('./ClaimButton.coffee');

ref = React.DOM, ul = ref.ul, li = ref.li, div = ref.div, h6 = ref.h6, button = ref.button, span = ref.span, code = ref.code;

recl = React.createClass;

rele = React.createElement;

ShopShips = Scry("/shop/:type/:nth", function(arg) {
  var ship, shop;
  shop = arg.shop;
  return ul({
    className: "shop"
  }, (function() {
    var i, len, results;
    results = [];
    for (i = 0, len = shop.length; i < len; i++) {
      ship = shop[i];
      results.push(li({
        className: "option",
        key: ship
      }, span({
        className: "mono"
      }, "~", ship, " "), rele(ClaimButton, {
        ship: ship
      })));
    }
    return results;
  })());
});

Shop = function(type, length) {
  return recl({
    displayName: "Shop-" + type,
    roll: function() {
      return {
        shipSelector: Math.floor(Math.random() * 10)
      };
    },
    reroll: function() {
      return this.setState(this.roll());
    },
    getInitialState: function() {
      return this.roll();
    },
    onInputShip: function(customShip) {
      return this.setState({
        customShip: customShip
      });
    },
    render: function() {
      var ref1;
      return div({}, h6({}, "Avaliable " + type + " — "), rele(ShopShips, _.extend({}, this.props, {
        type: type,
        nth: this.state.shipSelector
      })), button({
        onClick: this.reroll,
        className: 'reroll'
      }, "Get a new set"), h6({}, "Custom " + type + " — "), div({}, div({
        style: {
          marginBottom: ".3rem"
        }
      }, "If you understand how to pick a ", code({}, "@p"), " for " + type + ", feel free:"), div({
        style: {
          marginBottom: "1rem"
        }
      }, rele(ShipInput, {
        length: length,
        onInputShip: this.onInputShip
      }), rele(ClaimButton, {
        ship: (ref1 = this.state.customShip) != null ? ref1 : ""
      }))));
    }
  });
};

module.exports = Shop;


},{"./ClaimButton.coffee":6,"./Scry.coffee":13,"./ShipInput.coffee":14}],17:[function(require,module,exports){
var MainComponent, TreeActions;

MainComponent = require('./components/Main.coffee');

TreeActions = window.tree.actions;

TreeActions.registerComponent("womb", MainComponent);


},{"./components/Main.coffee":10}],18:[function(require,module,exports){
var PO, PO_old, SHIPSHAPE,
  slice = [].slice;

SHIPSHAPE = /^~?([a-z]{3}|[a-z]{6}(-[a-z]{6}){0,3}|[a-z]{6}(-[a-z]{6}){3}(--[a-z]{6}(-[a-z]{6}){3})+)$/;

PO_old = 'doz mar bin wan sam lit sig hid fid lis sog dir wac sab wis sib\nrig sol dop mod fog lid hop dar dor lor hod fol rin tog sil mir\nhol pas lac rov liv dal sat lib tab han tic pid tor bol fos dot\nlos dil for pil ram tir win tad bic dif roc wid bis das mid lop\nril nar dap mol san loc nov sit nid tip sic rop wit nat pan min\nrit pod mot tam tol sav pos nap nop som fin fon ban por wor sip\nron nor bot wic soc wat dol mag pic dav bid bal tim tas mal lig\nsiv tag pad sal div dac tan sid fab tar mon ran nis wol mis pal\nlas dis map rab tob rol lat lon nod nav fig nom nib pag sop ral\nbil had doc rid moc pac rav rip fal tod til tin hap mic fan pat\ntac lab mog sim son pin lom ric tap fir has bos bat poc hac tid\nhav sap lin dib hos dab bit bar rac par lod dos bor toc hil mac\ntom dig fil fas mit hob har mig hin rad mas hal rag lag fad top\nmop hab nil nos mil fop fam dat nol din hat nac ris fot rib hoc\nnim lar fit wal rap sar nal mos lan don dan lad dov riv bac pol\nlap tal pit nam bon ros ton fod pon sov noc sor lav mat mip fap\n\n/  /  /  /  /  /  /  /  /  /  /  /  /  /  /\nzod nec bud wes sev per sut let ful pen syt dur wep ser wyl sun\nryp syx dyr nup heb peg lup dep dys put lug hec ryt tyv syd nex\nlun mep lut sep pes del sul ped tem led tul met wen byn hex feb\npyl dul het mev rut tyl wyd tep bes dex sef wyc bur der nep pur\nrys reb den nut sub pet rul syn reg tyd sup sem wyn rec meg net\nsec mul nym tev web sum mut nyx rex teb fus hep ben mus wyx sym\nsel ruc dec wex syr wet dyl myn mes det bet bel tux tug myr pel\nsyp ter meb set dut deg tex sur fel tud nux rux ren wyt nub med\nlyt dus neb rum tyn seg lyx pun res red fun rev ref mec ted rus\nbex leb dux ryn num pyx ryg ryx fep tyr tus tyc leg nem fer mer\nten lus nus syl tec mex pub rym tuc fyl lep deb ber mug hut tun\nbyl sud pem dev lur def bus bep run mel pex dyt byt typ lev myl\nwed duc fur fex nul luc len ner lex rup ned lec ryd lyd fen wel\nnyd hus rel rud nes hes fet des ret dun ler nyr seb hul ryl lud\nrem lys fyn wer ryc sug nys nyl lyn dyn dem lux fed sed bec mun\nlyr tes mud nyt byr sen weg fyr mur tel rep teg pec nel nev fes';

PO = 'doz mar bin wan sam lit sig hid fid lis sog dir wac sab wis sib\nrig sol dop mod fog lid hop dar dor lor hod fol rin tog sil mir\nhol pas lac rov liv dal sat lib tab han tic pid tor bol fos dot\nlos dil for pil ram tir win tad bic dif roc wid bis das mid lop\nril nar dap mol san loc nov sit nid tip sic rop wit nat pan min\nrit pod mot tam tol sav pos nap nop som fin fon ban dor wor sip\nron nor bot wic soc wat dol mag pic dav bid bal tim tas mal lig\nsiv tag pad sal div dac tan sid fab tar mon ran nis wol mis pal\nlas dis map rab tob rol lat lon nod nav fig nom nib pag sop ral\nbil had doc rid moc pac rav rip fal tod til tin hap mic fan pat\ntac lab mog sim son pin lom ric tap fir has bos bat poc hac tid\nhav sap lin dib hos dab bit bar rac par lod dos bor toc hil mac\ntom dig fil fas mit hob har mig hin rad mas hal rag lag fad top\nmop hab nil nos mil fop fam dat nol din hat nac ris fot rib hoc\nnim lar fit wal rap sar nal mos lan don dan lad dov riv bac pol\nlap tal pit nam bon ros ton fod pon sov noc sor lav mat mip fip\n\nzod nec bud wes sev per sut let ful pen syt dur wep ser wyl sun\nryp syx dyr nup heb peg lup dep dys put lug hec ryt tyv syd nex\nlun mep lut sep pes del sul ped tem led tul met wen byn hex feb\npyl dul het mev rut tyl wyd tep bes dex sef wyc bur der nep pur\nrys reb den nut sub pet rul syn reg tyd sup sem wyn rec meg net\nsec mul nym tev web sum mut nyx rex teb fus hep ben mus wyx sym\nsel ruc dec wex syr wet dyl myn mes det bet bel tux tug myr pel\nsyp ter meb set dut deg tex sur fel tud nux rux ren wyt nub med\nlyt dus neb rum tyn seg lyx pun res red fun rev ref mec ted rus\nbex leb dux ryn num pyx ryg ryx fep tyr tus tyc leg nem fer mer\nten lus nus syl tec mex pub rym tuc fyl lep deb ber mug hut tun\nbyl sud pem dev lur def bus bep run mel pex dyt byt typ lev myl\nwed duc fur fex nul luc len ner lex rup ned lec ryd lyd fen wel\nnyd hus rel rud nes hes fet des ret dun ler nyr seb hul ryl lud\nrem lys fyn wer ryc sug nys nyl lyn dyn dem lux fed sed bec mun\nlyr tes mud nyt byr sen weg fyr mur tel rep teg pec nel nev fes';

module.exports = {
  unpackFrond: function(a) {
    var alts, key, ref;
    ref = _.keys(a), key = ref[0], alts = 2 <= ref.length ? slice.call(ref, 1) : [];
    if (!_.isEmpty(alts)) {
      throw new Error("Improper frond: " + ([key].concat(slice.call(alts)).join(',')));
    }
    return [key, a[key]];
  },
  uvShape: function(a) {
    return (a.slice(0, 2) === "0v") && /^[0-9a-v]{1,5}(\.[0-9a-v]{5})*$/.test(a.slice(2));
  },
  shipShape: function(a, old) {
    if (old == null) {
      old = false;
    }
    return (SHIPSHAPE.test(a)) && _.all(a.match(/[a-z]{3}/g), function(b) {
      return -1 !== (old ? PO_old : PO).indexOf(b);
    });
  },
  mailShape: function(a) {
    var valid;
    return valid = a.indexOf('@') !== -1 && a.indexOf('.') !== -1 && a.length > 7 && a.split(".").slice(-1)[0].length > 1 && a.split("@")[0].length > 0 && a.split("@")[1].length > 4;
  }
};


},{}],19:[function(require,module,exports){
/*!
  Copyright (c) 2016 Jed Watson.
  Licensed under the MIT License (MIT), see
  http://jedwatson.github.io/classnames
*/
/* global define */

(function () {
	'use strict';

	var hasOwn = {}.hasOwnProperty;

	function classNames () {
		var classes = [];

		for (var i = 0; i < arguments.length; i++) {
			var arg = arguments[i];
			if (!arg) continue;

			var argType = typeof arg;

			if (argType === 'string' || argType === 'number') {
				classes.push(arg);
			} else if (Array.isArray(arg)) {
				classes.push(classNames.apply(null, arg));
			} else if (argType === 'object') {
				for (var key in arg) {
					if (hasOwn.call(arg, key) && arg[key]) {
						classes.push(key);
					}
				}
			}
		}

		return classes.join(' ');
	}

	if (typeof module !== 'undefined' && module.exports) {
		module.exports = classNames;
	} else if (typeof define === 'function' && typeof define.amd === 'object' && define.amd) {
		// register as 'classnames', consistent with npm package name
		define('classnames', [], function () {
			return classNames;
		});
	} else {
		window.classNames = classNames;
	}
}());

},{}],20:[function(require,module,exports){
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
      } else {
        // At least give some kind of context to the user
        var err = new Error('Uncaught, unspecified "error" event. (' + er + ')');
        err.context = er;
        throw err;
      }
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

},{}]},{},[17]);
