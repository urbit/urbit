(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Matr, Prompt, Share, TreeActions, buffer, div, pre, recl, ref, ref1, rele, span, str, u,
  slice = [].slice,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

ref = [React.createClass, React.createElement], recl = ref[0], rele = ref[1];

ref1 = React.DOM, div = ref1.div, u = ref1.u, pre = ref1.pre, span = ref1.span;

TreeActions = window.tree.actions;

str = JSON.stringify;

Share = require("./share.coffee");

buffer = {
  "": new Share("")
};

Prompt = recl({
  displayName: "Prompt",
  render: function() {
    var buf, cur, pro, ref2, ref3;
    pro = (ref2 = this.props.prompt[this.props.appl]) != null ? ref2 : "X";
    cur = this.props.cursor;
    buf = this.props.input + " ";
    return pre({}, this.props.appl + pro, span({
      style: {
        background: 'lightgray'
      }
    }, buf.slice(0, cur), u({}, (ref3 = buf[cur]) != null ? ref3 : " "), buf.slice(cur + 1)));
  }
});

Matr = recl({
  displayName: "Matr",
  render: function() {
    var lines;
    lines = this.props.rows.map(function(lin, key) {
      return pre({
        key: key
      }, lin, " ");
    });
    lines.push(rele(Prompt, {
      key: "prompt",
      appl: this.props.appl,
      prompt: this.props.prompt,
      input: this.props.input,
      cursor: this.props.cursor
    }));
    return div({}, lines);
  }
});

TreeActions.registerComponent("sole", recl({
  displayName: "Sole",
  getInitialState: function() {
    return {
      rows: [],
      appl: this.props.appl,
      prompt: {
        "": "# "
      },
      input: "",
      cursor: 0,
      history: [],
      offset: 0,
      error: ""
    };
  },
  render: function() {
    return div({}, div({
      id: "err"
    }, this.state.error), rele(Matr, this.state));
  },
  flash: function($el, background) {
    $el.css({
      background: background
    });
    if (background) {
      return setTimeout(((function(_this) {
        return function() {
          return _this.flash($el, '');
        };
      })(this)), 50);
    }
  },
  bell: function() {
    return this.flash($('body'), 'black');
  },
  choose: function(appl) {
    if (buffer[appl] == null) {
      buffer[appl] = new Share("");
    }
    this.updPrompt('', null);
    return this.setState({
      appl: appl,
      cursor: 0,
      input: buffer[appl].buf
    });
  },
  print: function(txt) {
    return this.setState({
      rows: slice.call(this.state.rows).concat([txt])
    });
  },
  sync: function(ted, app) {
    var b;
    if (app == null) {
      app = this.state.appl;
    }
    if (app === this.state.appl) {
      b = buffer[app];
      return this.setState({
        input: b.buf,
        cursor: b.transpose(ted, this.state.cursor)
      });
    }
  },
  updPrompt: function(app, pro) {
    var prompt;
    prompt = $.extend({}, this.state.prompt);
    if (pro != null) {
      prompt[app] = pro;
    } else {
      delete prompt[app];
    }
    return this.setState({
      prompt: prompt
    });
  },
  sysStatus: function() {
    var app, k, pro, ref2, v;
    return this.updPrompt('', ((ref2 = [
      this.state.appl, (function() {
        var ref2, results;
        ref2 = this.state.prompt;
        results = [];
        for (k in ref2) {
          v = ref2[k];
          if (k !== '') {
            results.push(k);
          }
        }
        return results;
      }).call(this)
    ], app = ref2[0], pro = ref2[1], ref2), app === '' ? (pro.join(', ')) + '# ' : null));
  },
  peer: function(ruh, app) {
    var mapr, v;
    if (app == null) {
      app = this.state.appl;
    }
    if (ruh.map) {
      return ruh.map((function(_this) {
        return function(rul) {
          return _this.peer(rul, app);
        };
      })(this));
    }
    mapr = this.state;
    switch (Object.keys(ruh)[0]) {
      case 'txt':
        return this.print(ruh.txt);
      case 'tan':
        return ruh.tan.split("\n").map(this.print);
      case 'pro':
        return this.updPrompt(app, ruh.pro.cad);
      case 'hop':
        this.setState({
          cursor: ruh.hop
        });
        return this.bell();
      case 'blk':
        return console.log("Stub " + (str(ruh)));
      case 'det':
        buffer[app].receive(ruh.det);
        return this.sync(ruh.det.ted, app);
      case 'act':
        switch (ruh.act) {
          case 'clr':
            return this.setState({
              rows: []
            });
          case 'bel':
            return this.bell();
          case 'nex':
            return this.setState({
              input: "",
              cursor: 0,
              history: !mapr.input ? mapr.history : [mapr.input].concat(slice.call(mapr.history)),
              offset: 0
            });
        }
        break;
      default:
        v = Object.keys(ruh);
        return console.log(v, ruh[v[0]]);
    }
  },
  join: function(app) {
    if (this.state.prompt[app] != null) {
      return this.print('# already-joined: ' + app);
    }
    this.choose(app);
    return urb.bind("/sole", {
      appl: this.state.appl,
      wire: "/"
    }, (function(_this) {
      return function(err, d) {
        if (err) {
          return console.log(err);
        } else if (d.data) {
          return _this.peer(d.data, app);
        }
      };
    })(this));
  },
  cycle: function() {
    var apps, ref2;
    apps = Object.keys(this.state.prompt);
    if (apps.length < 2) {
      return;
    }
    return this.choose((ref2 = apps[1 + apps.indexOf(this.state.appl)]) != null ? ref2 : apps[0]);
  },
  part: function(appl) {
    var mapr;
    mapr = this.state;
    if (mapr.prompt[appl] == null) {
      return this.print('# not-joined: ' + appl);
    }
    urb.drop("/sole", {
      appl: appl,
      wire: "/"
    });
    if (appl === mapr.appl) {
      this.cycle();
    }
    this.updPrompt(appl, null);
    return this.sysStatus();
  },
  componentWillUnmount: function() {
    return this.mousetrapStop();
  },
  componentDidMount: function() {
    this.mousetrapInit();
    return this.join(this.state.appl);
  },
  sendAction: function(data) {
    var app, appl;
    appl = this.state.appl;
    if (appl) {
      return urb.send(data, {
        appl: appl,
        mark: 'sole-action'
      }, (function(_this) {
        return function(e, res) {
          if (res.status !== 200) {
            return _this.setState({
              error: res.data.mess
            });
          }
        };
      })(this));
    } else if (data === 'ret') {
      app = /^[a-z-]+$/.exec(buffer[""].buf.slice(1));
      if (!((app != null) && (app[0] != null))) {
        return this.bell();
      } else {
        switch (buffer[""].buf[0]) {
          case '+':
            this.doEdit({
              set: ""
            });
            return this.join(app[0]);
          case '-':
            this.doEdit({
              set: ""
            });
            return this.part(app[0]);
          default:
            return this.bell();
        }
      }
    }
  },
  doEdit: function(ted) {
    var det;
    det = buffer[this.state.appl].transmit(ted);
    this.sync(ted);
    return this.sendAction({
      det: det
    });
  },
  eatKyev: function(mod, key) {
    var _, appl, cha, cursor, history, input, mapr, n, offset, prev, ref2, ref3, rest;
    mapr = this.state;
    switch (mod.sort().join('-')) {
      case '':
      case 'shift':
        if (key.str) {
          this.doEdit({
            ins: {
              cha: key.str,
              at: mapr.cursor
            }
          });
          this.setState({
            cursor: mapr.cursor + 1
          });
        }
        switch (key.act) {
          case 'entr':
            return this.sendAction('ret');
          case 'up':
            history = mapr.history.slice();
            offset = mapr.offset;
            if (history[offset] === void 0) {
              return;
            }
            ref2 = [history[offset], mapr.input], input = ref2[0], history[offset] = ref2[1];
            offset++;
            this.doEdit({
              set: input
            });
            return this.setState({
              offset: offset,
              history: history,
              cursor: input.length
            });
          case 'down':
            history = mapr.history.slice();
            offset = mapr.offset;
            offset--;
            if (history[offset] === void 0) {
              return;
            }
            ref3 = [history[offset], mapr.input], input = ref3[0], history[offset] = ref3[1];
            this.doEdit({
              set: input
            });
            return this.setState({
              offset: offset,
              history: history,
              cursor: input.length
            });
          case 'left':
            if (mapr.cursor > 0) {
              return this.setState({
                cursor: mapr.cursor - 1
              });
            }
            break;
          case 'right':
            if (mapr.cursor < mapr.input.length) {
              return this.setState({
                cursor: mapr.cursor + 1
              });
            }
            break;
          case 'baxp':
            if (mapr.cursor > 0) {
              return this.doEdit({
                del: mapr.cursor - 1
              });
            }
        }
        break;
      case 'ctrl':
        switch (key.str || key.act) {
          case 'a':
          case 'left':
            return this.setState({
              cursor: 0
            });
          case 'e':
          case 'right':
            return this.setState({
              cursor: mapr.input.length
            });
          case 'l':
            return this.setState({
              rows: []
            });
          case 'entr':
            return this.bell();
          case 'w':
            return this.eatKyev(['alt'], {
              act: 'baxp'
            });
          case 'p':
            return this.eatKyev([], {
              act: 'up'
            });
          case 'n':
            return this.eatKyev([], {
              act: 'down'
            });
          case 'b':
            return this.eatKyev([], {
              act: 'left'
            });
          case 'f':
            return this.eatKyev([], {
              act: 'right'
            });
          case 'g':
            return this.bell();
          case 'x':
            return this.cycle();
          case 'v':
            appl = mapr.appl !== '' ? '' : this.state.appl;
            this.setState({
              appl: appl,
              cursor: 0,
              input: buffer[appl].buf
            });
            return this.sysStatus();
          case 't':
            if (mapr.cursor === 0 || mapr.input.length < 2) {
              return this.bell();
            }
            cursor = mapr.cursor;
            if (cursor < mapr.input.length) {
              cursor++;
            }
            this.doEdit([
              {
                del: cursor - 1
              }, {
                ins: {
                  at: cursor - 2,
                  cha: mapr.input[cursor - 1]
                }
              }
            ]);
            return this.setState({
              cursor: cursor
            });
          case 'u':
            this.yank = mapr.input.slice(0, mapr.cursor);
            return this.doEdit((function() {
              var i, ref4, results;
              results = [];
              for (n = i = 1, ref4 = mapr.cursor; 1 <= ref4 ? i <= ref4 : i >= ref4; n = 1 <= ref4 ? ++i : --i) {
                results.push({
                  del: mapr.cursor - n
                });
              }
              return results;
            })());
          case 'k':
            this.yank = mapr.input.slice(mapr.cursor);
            return this.doEdit((function() {
              var i, ref4, ref5, results;
              results = [];
              for (_ = i = ref4 = mapr.cursor, ref5 = mapr.input.length; ref4 <= ref5 ? i < ref5 : i > ref5; _ = ref4 <= ref5 ? ++i : --i) {
                results.push({
                  del: mapr.cursor
                });
              }
              return results;
            })());
          case 'y':
            return this.doEdit((function() {
              var i, len, ref4, ref5, results;
              ref5 = (ref4 = this.yank) != null ? ref4 : '';
              results = [];
              for (n = i = 0, len = ref5.length; i < len; n = ++i) {
                cha = ref5[n];
                results.push({
                  ins: {
                    cha: cha,
                    at: mapr.cursor + n
                  }
                });
              }
              return results;
            }).call(this));
          default:
            return console.log(mod, str(key));
        }
        break;
      case 'alt':
        switch (key.str || key.act) {
          case 'f':
          case 'right':
            rest = mapr.input.slice(mapr.cursor);
            rest = rest.match(/\W*\w*/)[0];
            return this.setState({
              cursor: mapr.cursor + rest.length
            });
          case 'b':
          case 'left':
            prev = mapr.input.slice(0, mapr.cursor);
            prev = prev.split('').reverse().join('');
            prev = prev.match(/\W*\w*/)[0];
            return this.setState({
              cursor: mapr.cursor - prev.length
            });
          case 'baxp':
            prev = mapr.input.slice(0, mapr.cursor);
            prev = prev.split('').reverse().join('');
            prev = prev.match(/\W*\w*/)[0];
            this.yank = prev;
            return this.doEdit((function() {
              var i, len, results;
              results = [];
              for (n = i = 0, len = prev.length; i < len; n = ++i) {
                _ = prev[n];
                results.push({
                  del: mapr.cursor - 1 - n
                });
              }
              return results;
            })());
        }
        break;
      default:
        return console.log(mod, str(key));
    }
  },
  mousetrapStop: function() {
    return Mousetrap.handleKey = this._defaultHandleKey;
  },
  mousetrapInit: function() {
    this._defaultHandleKey = Mousetrap.handleKey;
    return Mousetrap.handleKey = (function(_this) {
      return function(char, mod, e) {
        var chac, key, norm, ref2;
        norm = {
          capslock: 'caps',
          pageup: 'pgup',
          pagedown: 'pgdn',
          backspace: 'baxp',
          enter: 'entr'
        };
        key = (function() {
          var ref2;
          switch (false) {
            case char.length !== 1:
              if (e.type === 'keypress') {
                chac = char.charCodeAt(0);
                if (chac < 32) {
                  char = String.fromCharCode(chac | 96);
                }
                return {
                  str: char
                };
              }
              break;
            case e.type !== 'keydown':
              if (char !== 'space') {
                return {
                  act: (ref2 = norm[char]) != null ? ref2 : char
                };
              }
              break;
            case !(e.type === 'keyup' && norm[key] === 'caps'):
              return {
                act: 'uncap'
              };
          }
        })();
        if (!key) {
          return;
        }
        if (key.act && (ref2 = key.act, indexOf.call(mod, ref2) >= 0)) {
          return;
        }
        e.preventDefault();
        return _this.eatKyev(mod, key);
      };
    })(this);
  }
}));


},{"./share.coffee":2}],2:[function(require,module,exports){
var clog, str;

str = JSON.stringify;

clog = function(a) {
  return console.log(a);
};

module.exports = (function() {
  function exports(buf, ven, leg) {
    this.buf = buf != null ? buf : "";
    this.ven = ven != null ? ven : [0, 0];
    this.leg = leg != null ? leg : [];
  }

  exports.prototype.abet = function() {
    return {
      buf: this.buf,
      leg: this.leg.slice(),
      ven: this.ven.slice()
    };
  };

  exports.prototype.apply = function(ted) {
    var at, cha, ref;
    switch (false) {
      case 'nop' !== ted:
        break;
      case !ted.map:
        return ted.map(this.apply, this);
      default:
        switch (Object.keys(ted)[0]) {
          case 'set':
            return this.buf = ted.set;
          case 'del':
            return this.buf = this.buf.slice(0, ted.del) + this.buf.slice(ted.del + 1);
          case 'ins':
            ref = ted.ins, at = ref.at, cha = ref.cha;
            return this.buf = this.buf.slice(0, at) + cha + this.buf.slice(at);
          default:
            throw "%sole-edit -lost." + (str(ted));
        }
    }
  };

  exports.prototype.transmute = function(sin, dex) {
    var at, cha, ref;
    switch (false) {
      case !(sin === 'nop' || dex === 'nop'):
        return dex;
      case !sin.reduce:
        return sin.reduce(((function(_this) {
          return function(dex, syn) {
            return _this.transmute(syn, dex);
          };
        })(this)), dex);
      case !dex.map:
        return dex.map((function(_this) {
          return function(dax) {
            return _this.transmute(sin, dax);
          };
        })(this));
      case dex.set === void 0:
        return dex;
      default:
        switch (Object.keys(sin)[0]) {
          case 'set':
            return 'nop';
          case 'del':
            if (sin.del === dex.del) {
              return 'nop';
            }
            dex = $.extend(true, {}, dex);
            switch (Object.keys(dex)[0]) {
              case 'del':
                if (sin.del < dex.del) {
                  dex.del--;
                }
                break;
              case 'ins':
                if (sin.del < dex.ins.at) {
                  dex.ins.at--;
                }
            }
            return dex;
          case 'ins':
            dex = $.extend(true, {}, dex);
            ref = sin.ins, at = ref.at, cha = ref.cha;
            switch (Object.keys(dex)[0]) {
              case 'del':
                if (at < dex.del) {
                  dex.del++;
                }
                break;
              case 'ins':
                if (at < dex.ins.at || (at === dex.ins.at && !(cha <= dex.ins.cha))) {
                  dex.ins.at++;
                }
            }
            return dex;
          default:
            throw "%sole-edit -lost." + (str(sin));
        }
    }
  };

  exports.prototype.commit = function(ted) {
    this.ven[0]++;
    this.leg.push(ted);
    return this.apply(ted);
  };

  exports.prototype.inverse = function(ted) {
    switch (false) {
      case 'nop' !== ted:
        return ted;
      case !ted.map:
        return ted.map((function(_this) {
          return function(tad) {
            var res;
            res = _this.inverse(tad);
            _this.apply(tad);
            return res;
          };
        })(this)).reverse();
      default:
        switch (Object.keys(ted)[0]) {
          case 'set':
            return {
              set: this.buf
            };
          case 'ins':
            return {
              del: ted.ins
            };
          case 'del':
            return {
              ins: {
                at: ted.del,
                cha: this.buf[ted.del]
              }
            };
          default:
            throw "%sole-edit -lost." + (str(ted));
        }
    }
  };

  exports.prototype.receive = function(arg) {
    var dat, ler, ted;
    ler = arg.ler, ted = arg.ted;
    if (!(ler[1] === this.ven[1])) {
      throw "-out-of-sync.[" + (str(ler)) + " " + (str(this.ven)) + "]";
    }
    this.leg = this.leg.slice(this.leg.length + ler[0] - this.ven[0]);
    dat = this.transmute(this.leg, ted);
    this.ven[1]++;
    this.apply(dat);
    return dat;
  };

  exports.prototype.remit = function() {
    throw 'stub';
  };

  exports.prototype.transmit = function(ted) {
    var act;
    act = {
      ted: ted,
      ler: [this.ven[1], this.ven[0]]
    };
    this.commit(ted);
    return act;
  };

  exports.prototype.transceive = function(arg) {
    var dat, ler, old, ted;
    ler = arg.ler, ted = arg.ted;
    old = new Share(this.buf);
    dat = this.receive({
      ler: ler,
      ted: ted
    });
    return old.inverse(dat);
  };

  exports.prototype.transpose = function(ted, pos) {
    var ref;
    if (pos === void 0) {
      return this.transpose(this.leg, ted);
    } else {
      return ((ref = (this.transmute(ted, {
        ins: {
          at: pos
        }
      })).ins) != null ? ref : {
        at: 0
      }).at;
    }
  };

  return exports;

})();


},{}]},{},[1]);
