(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var DOM, Matr, Prompt, Share, div, pre, recl, ref, ref1, rend, span, str,
  slice = [].slice,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

ref = [React.DOM, React.createClass, React.renderComponent], DOM = ref[0], recl = ref[1], rend = ref[2];

ref1 = [DOM.div, DOM.pre, DOM.span], div = ref1[0], pre = ref1[1], span = ref1[2];

str = JSON.stringify;

Share = require("./share.coffee");

Prompt = recl({
  render: function() {
    var buf, cur, pro, ref2, ref3;
    ref3 = [(ref2 = this.props.prompt[this.props.appl]) != null ? ref2 : "X", this.props.cursor, this.props.input + " "], pro = ref3[0], cur = ref3[1], buf = ref3[2];
    return pre({}, this.props.appl + pro, span({
      style: {
        background: 'lightgray'
      }
    }, buf.slice(0, cur), "\u0332", buf.slice(cur)));
  }
});

Matr = recl({
  render: function() {
    var lines;
    lines = this.props.rows.map(function(lin) {
      return pre({}, lin, " ");
    });
    lines.push(Prompt({
      appl: this.props.appl,
      prompt: this.props.prompt,
      input: this.props.input,
      cursor: this.props.cursor
    }));
    return div({}, lines);
  }
});

$(function() {
  var bell, buffer, choose, cycle, deltim, doEdit, eatKyev, flash, join, matr, met, part, peer, pressed, print, sendAction, subs, sync, sysStatus, updPrompt, update, yank;
  met = $('<pre>').text('m').css({
    display: 'none'
  }).appendTo(term).width();
  subs = "";
  flash = function($el, background) {
    $el.css({
      background: background
    });
    if (background) {
      return setTimeout((function() {
        return flash($el, '');
      }), 50);
    }
  };
  bell = function() {
    return flash($('body'), 'black');
  };
  matr = rend(Matr({
    rows: [],
    appl: "",
    prompt: {
      "": "# "
    },
    input: "",
    cursor: 0,
    history: [],
    offset: 0
  }), term);
  window.matr = matr;
  update = function(a) {
    return matr.setProps(a);
  };
  buffer = {
    "": new Share("")
  };
  window.buffer = buffer;
  choose = function(appl) {
    urb.appl = appl;
    if (buffer[appl] == null) {
      buffer[appl] = new Share("");
    }
    updPrompt('', null);
    return update({
      appl: appl,
      cursor: 0,
      input: buffer[appl].buf
    });
  };
  print = function(txt) {
    return update({
      rows: slice.call(matr.props.rows).concat([txt])
    });
  };
  sync = function(ted, app) {
    var b;
    if (app == null) {
      app = matr.props.appl;
    }
    if (app !== matr.props.appl) {
      return;
    }
    b = buffer[app];
    return update({
      input: b.buf,
      cursor: b.transpose(ted, matr.props.cursor)
    });
  };
  updPrompt = function(app, pro) {
    var prompt;
    prompt = $.extend({}, matr.props.prompt);
    if (pro != null) {
      prompt[app] = pro;
    } else {
      delete prompt[app];
    }
    return update({
      prompt: prompt
    });
  };
  sysStatus = function() {
    var app, k, pro, ref2, v;
    return updPrompt('', ((ref2 = [
      matr.props.appl, (function() {
        var ref2, results;
        ref2 = matr.props.prompt;
        results = [];
        for (k in ref2) {
          v = ref2[k];
          if (k !== '') {
            results.push(k);
          }
        }
        return results;
      })()
    ], app = ref2[0], pro = ref2[1], ref2), app === '' ? (pro.join(', ')) + '# ' : null));
  };
  peer = function(ruh, app) {
    var mapr, v;
    if (app == null) {
      app = urb.appl;
    }
    if (ruh.map) {
      return ruh.map(function(rul) {
        return peer(rul, app);
      });
    }
    mapr = matr.props;
    switch (Object.keys(ruh)[0]) {
      case 'txt':
        return print(ruh.txt);
      case 'tan':
        return ruh.tan.split("\n").map(print);
      case 'pro':
        return updPrompt(app, ruh.pro.cad);
      case 'hop':
        update({
          cursor: ruh.hop
        });
        return bell();
      case 'blk':
        return console.log("Stub " + (str(ruh)));
      case 'det':
        buffer[app].receive(ruh.det);
        return sync(ruh.det.ted, app);
      case 'act':
        switch (ruh.act) {
          case 'clr':
            return update({
              rows: []
            });
          case 'bel':
            return bell();
          case 'nex':
            return update({
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
  };
  join = function(app) {
    if (matr.props.prompt[app] != null) {
      return print('# already-joined: ' + app);
    }
    choose(app);
    return urb.bind("/sole", {
      wire: "/"
    }, function(err, d) {
      if (err) {
        return console.log(err);
      } else if (d.data) {
        return peer(d.data, app);
      }
    });
  };
  cycle = function() {
    var apps, ref2;
    apps = Object.keys(matr.props.prompt);
    if (apps.length < 2) {
      return;
    }
    return choose((ref2 = apps[1 + apps.indexOf(urb.appl)]) != null ? ref2 : apps[0]);
  };
  part = function(appl) {
    var mapr;
    mapr = matr.props;
    if (mapr.prompt[appl] == null) {
      return print('# not-joined: ' + appl);
    }
    urb.drop("/sole", {
      appl: appl,
      wire: "/"
    });
    if (appl === mapr.appl) {
      cycle();
    }
    updPrompt(appl, null);
    return sysStatus();
  };
  join(urb.appl);
  window.join = join;
  window.part = part;
  pressed = [];
  deltim = null;
  urb.send.mark = 'sole-action';
  sendAction = function(data) {
    var app;
    if (matr.props.appl) {
      return urb.send(data, function(e, res) {
        if (res.status !== 200) {
          return $('#err')[0].innerText = res.data.mess;
        }
      });
    } else if (data === 'ret') {
      app = /^[a-z-]+$/.exec(buffer[""].buf.slice(1));
      if (!((app != null) && (app[0] != null))) {
        return bell();
      } else {
        switch (buffer[""].buf[0]) {
          case '+':
            doEdit({
              set: ""
            });
            return join(app[0]);
          case '-':
            doEdit({
              set: ""
            });
            return part(app[0]);
          default:
            return bell();
        }
      }
    }
  };
  doEdit = function(ted) {
    var det;
    det = buffer[matr.props.appl].transmit(ted);
    sync(ted);
    return sendAction({
      det: det
    });
  };
  yank = '';
  eatKyev = function(mod, key) {
    var _, appl, cha, cursor, history, input, mapr, n, offset, prev, ref2, ref3, rest;
    mapr = matr.props;
    switch (mod.sort().join('-')) {
      case '':
      case 'shift':
        if (key.str) {
          doEdit({
            ins: {
              cha: key.str,
              at: mapr.cursor
            }
          });
          update({
            cursor: mapr.cursor + 1
          });
        }
        switch (key.act) {
          case 'entr':
            return sendAction('ret');
          case 'up':
            history = mapr.history.slice();
            offset = mapr.offset;
            if (history[offset] === void 0) {
              return;
            }
            ref2 = [history[offset], mapr.input], input = ref2[0], history[offset] = ref2[1];
            offset++;
            doEdit({
              set: input
            });
            return update({
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
            doEdit({
              set: input
            });
            return update({
              offset: offset,
              history: history,
              cursor: input.length
            });
          case 'left':
            if (mapr.cursor > 0) {
              return update({
                cursor: mapr.cursor - 1
              });
            }
            break;
          case 'right':
            if (mapr.cursor < mapr.input.length) {
              return update({
                cursor: mapr.cursor + 1
              });
            }
            break;
          case 'baxp':
            if (mapr.cursor > 0) {
              return doEdit({
                del: mapr.cursor - 1
              });
            }
        }
        break;
      case 'ctrl':
        switch (key.str || key.act) {
          case 'a':
          case 'left':
            return update({
              cursor: 0
            });
          case 'e':
          case 'right':
            return update({
              cursor: mapr.input.length
            });
          case 'l':
            return update({
              rows: []
            });
          case 'entr':
            return bell();
          case 'w':
            return eatKyev(['alt'], {
              act: 'baxp'
            });
          case 'p':
            return eatKyev([], {
              act: 'up'
            });
          case 'n':
            return eatKyev([], {
              act: 'down'
            });
          case 'b':
            return eatKyev([], {
              act: 'left'
            });
          case 'f':
            return eatKyev([], {
              act: 'right'
            });
          case 'g':
            return bell();
          case 'x':
            return cycle();
          case 'v':
            appl = mapr.appl !== '' ? '' : urb.appl;
            update({
              appl: appl,
              cursor: 0,
              input: buffer[appl].buf
            });
            return sysStatus();
          case 't':
            if (mapr.cursor === 0 || mapr.input.length < 2) {
              return bell();
            }
            cursor = mapr.cursor;
            if (cursor < mapr.input.length) {
              cursor++;
            }
            doEdit([
              {
                del: cursor - 1
              }, {
                ins: {
                  at: cursor - 2,
                  cha: mapr.input[cursor - 1]
                }
              }
            ]);
            return update({
              cursor: cursor
            });
          case 'u':
            yank = mapr.input.slice(0, mapr.cursor);
            return doEdit((function() {
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
            yank = mapr.input.slice(mapr.cursor);
            return doEdit((function() {
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
            return doEdit((function() {
              var i, len, results;
              results = [];
              for (n = i = 0, len = yank.length; i < len; n = ++i) {
                cha = yank[n];
                results.push({
                  ins: {
                    cha: cha,
                    at: mapr.cursor + n
                  }
                });
              }
              return results;
            })());
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
            return update({
              cursor: mapr.cursor + rest.length
            });
          case 'b':
          case 'left':
            prev = mapr.input.slice(0, mapr.cursor);
            prev = prev.split('').reverse().join('');
            prev = prev.match(/\W*\w*/)[0];
            return update({
              cursor: mapr.cursor - prev.length
            });
          case 'baxp':
            prev = mapr.input.slice(0, mapr.cursor);
            prev = prev.split('').reverse().join('');
            prev = prev.match(/\W*\w*/)[0];
            yank = prev;
            return doEdit((function() {
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
  };
  return Mousetrap.handleKey = function(char, mod, e) {
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
    return eatKyev(mod, key);
  };
});


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
