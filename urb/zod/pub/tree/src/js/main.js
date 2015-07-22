(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var TreeDispatcher, TreePersistence;

TreeDispatcher = require('../dispatcher/Dispatcher.coffee');

TreePersistence = require('../persistence/TreePersistence.coffee');

module.exports = {
  loadPath: function(path, body, kids) {
    return TreeDispatcher.handleServerAction({
      type: "path-load",
      path: path,
      body: body,
      kids: kids
    });
  },
  loadKids: function(path, kids) {
    return TreeDispatcher.handleServerAction({
      type: "kids-load",
      path: path,
      kids: kids
    });
  },
  loadSnip: function(path, snip) {
    return TreeDispatcher.handleServerAction({
      type: "snip-load",
      path: path,
      snip: snip
    });
  },
  getPath: function(path, query) {
    if (path.slice(-1) === "/") {
      path = path.slice(0, -1);
    }
    return TreePersistence.get(path, query, (function(_this) {
      return function(err, res) {
        switch (query) {
          case "snip":
            return _this.loadSnip(path, res.snip);
          case "kids":
            return _this.loadKids(path, res.kids);
          default:
            return _this.loadPath(path, res.body, res.kids, res.snip);
        }
      };
    })(this));
  },
  setCurr: function(path) {
    return TreeDispatcher.handleViewAction({
      type: "set-curr",
      path: path
    });
  }
};



},{"../dispatcher/Dispatcher.coffee":8,"../persistence/TreePersistence.coffee":13}],2:[function(require,module,exports){
var BodyComponent, TreeActions, TreeStore, a, div, recl, ref,
  slice = [].slice;

BodyComponent = require('./BodyComponent.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a], div = ref[0], a = ref[1];

module.exports = recl({
  displayName: "Anchor",
  stateFromStore: function() {
    return {
      path: TreeStore.getCurr(),
      pare: TreeStore.getPare(),
      sibs: TreeStore.getSiblings(),
      snip: TreeStore.getSnip(),
      next: TreeStore.getNext(),
      prev: TreeStore.getPrev(),
      kids: TreeStore.getKids(),
      tree: TreeStore.getTree([]),
      cont: TreeStore.getCont(),
      url: window.location.pathname
    };
  },
  checkPath: function(path) {
    return this.state.cont[path] != null;
  },
  toggleFocus: function(state) {
    return $(this.getDOMNode()).toggleClass('focus', state);
  },
  _click: function() {
    return this.toggleFocus();
  },
  _mouseOver: function() {
    return this.toggleFocus(true);
  },
  _mouseOut: function() {
    return this.toggleFocus(false);
  },
  _touchStart: function() {
    return this.ts = Number(Date.now());
  },
  _touchEnd: function() {
    var dt;
    return dt = this.ts - Number(Date.now());
  },
  setPath: function(href, hist) {
    var href_parts, next, rend;
    href_parts = href.split("#");
    next = href_parts[0];
    if (next.substr(-1) === "/") {
      next = next.slice(0, -1);
    }
    href_parts[0] = next;
    if (hist !== false) {
      history.pushState({}, "", window.tree.basepath(href_parts.join("")));
    }
    rend = false;
    if (next !== this.state.path) {
      React.unmountComponentAtNode($('#cont')[0]);
      rend = true;
    }
    TreeActions.setCurr(next);
    if (rend === true) {
      return React.render(BodyComponent({}, ""), $('#cont')[0]);
    }
  },
  goTo: function(path) {
    var frag;
    this.toggleFocus(false);
    $("html,body").animate({
      scrollTop: 0
    });
    frag = path.split("#")[0];
    this.setPath(path);
    if (!this.checkPath(frag)) {
      return TreeActions.getPath(frag);
    }
  },
  checkURL: function() {
    if (this.state.url !== window.location.pathname) {
      return this.setPath(window.tree.fragpath(window.location.pathname), false);
    }
  },
  setTitle: function() {
    var path, title;
    title = $('#cont h1').first().text();
    if (title.length === 0) {
      path = this.state.path.split("/");
      title = path[path.length - 1];
    }
    return document.title = title + " - " + this.state.path;
  },
  checkUp: function() {
    var ref1, up;
    up = (ref1 = this.state.pare) != null ? ref1 : "/";
    if (up.slice(-1) === "/") {
      up = up.slice(0, -1);
    }
    if (!this.state.cont[up]) {
      return TreeActions.getPath(up);
    }
  },
  componentDidUpdate: function() {
    this.setTitle();
    return this.checkUp();
  },
  componentDidMount: function() {
    TreeStore.addChangeListener(this._onChangeStore);
    this.setTitle();
    this.checkUp();
    this.interval = setInterval(this.checkURL, 100);
    $('body').on('keyup', (function(_this) {
      return function(e) {
        if (e.keyCode === 37) {
          _this.goTo(_this.state.prev);
        }
        if (e.keyCode === 39) {
          return _this.goTo(_this.state.next);
        }
      };
    })(this));
    return $('body').on('click', 'a', (function(_this) {
      return function(e) {
        var href;
        href = $(e.target).closest('a').attr('href');
        if (href[0] === "/") {
          e.preventDefault();
          e.stopPropagation();
          return _this.goTo(window.tree.fragpath(href));
        }
      };
    })(this));
  },
  componentWillUnmount: function() {
    clearInterval(this.interval);
    return $('body').off('click', 'a');
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  renderArrow: function(name, path) {
    var href;
    href = window.tree.basepath(path);
    return a({
      href: href,
      key: "arow-" + name,
      className: "arow-" + name
    }, "");
  },
  renderParts: function() {
    var _sibs, ci, curr, j, k, ref1, style, up;
    return [
      this.state.pare ? _.filter([
        div({
          id: "up",
          key: "up"
        }, this.renderArrow("up", this.state.pare)), this.state.prev || this.state.next ? div({
          id: "sides",
          key: "sides"
        }, _.filter([this.state.prev ? this.renderArrow("prev", this.state.prev) : void 0, this.state.next ? this.renderArrow("next", this.state.next) : void 0])) : void 0
      ]) : void 0, _.keys(this.state.sibs).length > 0 ? ((ref1 = this.state.path.split("/"), up = 2 <= ref1.length ? slice.call(ref1, 0, j = ref1.length - 1) : (j = 0, []), curr = ref1[j++], ref1), up = up.join("/"), ci = 0, k = 0, _sibs = _(this.state.sibs).keys().sort().map((function(_this) {
        return function(i) {
          var className, head, href, path, ref2, ref3;
          if (curr === i) {
            className = "active";
            ci = k;
          }
          if (className == null) {
            className = "";
          }
          k++;
          path = up + "/" + i;
          href = window.tree.basepath(path);
          head = (ref2 = (ref3 = _this.state.snip[path]) != null ? ref3.head : void 0) != null ? ref2 : div({}, i);
          head = $(React.renderToStaticMarkup(head)).text();
          return div({
            className: className,
            key: i
          }, a({
            href: href,
            onClick: _this._click
          }, head));
        };
      })(this)), style = {
        marginTop: (-24 * ci) + "px"
      }, div({
        key: "sibs",
        id: "sibs",
        style: style
      }, _sibs)) : void 0
    ];
  },
  render: function() {
    var obj;
    obj = {
      onMouseOver: this._mouseOver,
      onMouseOut: this._mouseOut,
      onClick: this._click,
      onTouchStart: this._touchStart,
      onTouchEnd: this._touchEnd
    };
    if (_.keys(window).indexOf("ontouchstart") !== -1) {
      delete obj.onMouseOver;
      delete obj.onMouseOut;
    }
    return div(obj, _.filter(this.renderParts()));
  }
});



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":14,"./BodyComponent.coffee":3}],3:[function(require,module,exports){
var TreeActions, TreeStore, div, input, load, recl, ref, textarea;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

load = require('./LoadComponent.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.input, React.DOM.textarea], div = ref[0], input = ref[1], textarea = ref[2];

module.exports = recl({
  displayName: "Body",
  stateFromStore: function() {
    return {
      body: TreeStore.getBody(),
      curr: TreeStore.getCurr(),
      cont: TreeStore.getCont()
    };
  },
  componentDidMount: function() {
    return TreeStore.addChangeListener(this._onChangeStore);
  },
  componentWillUnmount: function() {
    return TreeStore.removeChangeListener(this._onChangeStore);
  },
  componentDidUpdate: function(_props, _state) {
    if (_state.curr !== this.state.curr) {
      return setTimeout(((function(_this) {
        return function() {
          return _this.getPath(_state.curr);
        };
      })(this)), 0);
    }
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  getPath: function(path) {
    if (this.state.cont[path] == null) {
      return TreeActions.getPath(path);
    }
  },
  render: function() {
    var ref1;
    return div({}, div({
      id: 'body',
      key: "body" + this.state.curr
    }, (ref1 = this.state.body) != null ? ref1 : div({
      className: "loading"
    }, load({}, ""))));
  }
});



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":14,"./LoadComponent.coffee":7}],4:[function(require,module,exports){
var div, recl, ref, textarea;

recl = React.createClass;

ref = [React.DOM.div, React.DOM.textarea], div = ref[0], textarea = ref[1];

module.exports = recl({
  render: function() {
    return div({}, textarea({
      ref: 'ed',
      value: this.props.value
    }));
  },
  componentDidMount: function() {
    return CodeMirror.fromTextArea(this.refs.ed.getDOMNode(), {
      readOnly: true,
      lineNumbers: true
    });
  }
});



},{}],5:[function(require,module,exports){
var TreeActions, TreeStore, a, div, hr, li, recl, ref, ul;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a, React.DOM.ul, React.DOM.li, React.DOM.hr], div = ref[0], a = ref[1], ul = ref[2], li = ref[3], hr = ref[4];

module.exports = recl({
  displayName: "Kids",
  stateFromStore: function() {
    var path, ref1;
    path = (ref1 = this.props.dataPath) != null ? ref1 : TreeStore.getCurr();
    return {
      path: path,
      cont: TreeStore.getCont(),
      tree: TreeStore.getTree(path.split("/"))
    };
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  gotPath: function() {
    var _keys;
    _keys = _(this.state.tree).keys();
    return (!_keys.isEmpty()) && _keys.every((function(_this) {
      return function(k) {
        return _this.state.cont[_this.state.path + "/" + k] != null;
      };
    })(this));
  },
  componentDidMount: function() {
    TreeStore.addChangeListener(this._onChangeStore);
    if (!this.gotPath()) {
      return TreeActions.getPath(this.state.path, "kids");
    }
  },
  render: function() {
    var v;
    return div({
      key: "kids-" + this.state.path,
      className: "kids"
    }, (function() {
      var i, len, ref1, results;
      ref1 = _.keys(this.state.tree).sort();
      results = [];
      for (i = 0, len = ref1.length; i < len; i++) {
        v = ref1[i];
        results.push([
          div({
            key: v
          }, this.state.cont[this.state.path + "/" + v]), hr({}, "")
        ]);
      }
      return results;
    }).call(this));
  }
});



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":14}],6:[function(require,module,exports){
var TreeActions, TreeStore, a, div, h1, li, load, recl, ref, ul;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

load = React.createFactory(require('./LoadComponent.coffee'));

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a, React.DOM.ul, React.DOM.li, React.DOM.h1], div = ref[0], a = ref[1], ul = ref[2], li = ref[3], h1 = ref[4];

module.exports = recl({
  displayName: "List",
  stateFromStore: function() {
    var path, ref1;
    path = (ref1 = this.props.dataPath) != null ? ref1 : TreeStore.getCurr();
    return {
      path: path,
      snip: TreeStore.getSnip(),
      tree: TreeStore.getTree(path.split("/"))
    };
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  componentWillUnmount: function() {
    return TreeStore.removeChangeListener(this._onChangeStore);
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  gotPath: function() {
    var _keys;
    _keys = _(this.state.tree).keys();
    return (!_keys.isEmpty()) && _keys.every((function(_this) {
      return function(k) {
        return _this.state.snip[_this.state.path + "/" + k] != null;
      };
    })(this));
  },
  componentDidMount: function() {
    TreeStore.addChangeListener(this._onChangeStore);
    if (!this.gotPath()) {
      return TreeActions.getPath(this.state.path, "snip");
    }
  },
  renderList: function() {
    var _k, _keys, _path, c, href, i, len, orig, prev, results, v;
    if (!this.gotPath()) {
      return div({
        className: "loading"
      }, load({}, ""));
    }
    _keys = _.keys(this.state.tree).sort();
    if (this.props.dataType === 'post') {
      _keys = _keys.reverse();
    }
    results = [];
    for (i = 0, len = _keys.length; i < len; i++) {
      v = _keys[i];
      _k = "";
      _path = this.state.path + "/" + v;
      if (this.props.dataPreview != null) {
        c = "preview";
        prev = this.state.snip[_path];
        if (this.props.titlesOnly) {
          prev = prev.head;
        } else {
          prev = [prev.head, prev.body];
        }
        if (this.props.dataType === 'post') {
          orig = this.state.snip[_path].orig;
          c = orig.body.c.slice(0, 2);
          c.unshift(orig.head);
          prev = {
            gn: 'div',
            c: c
          };
          _k += " post";
          prev = window.tree.reactify(prev);
        }
      } else {
        c = "";
        prev = h1({}, v);
      }
      href = window.tree.basepath(_path);
      results.push(li({
        className: _k,
        key: "list-a-" + _path
      }, a({
        href: href,
        className: c
      }, prev)));
    }
    return results;
  },
  render: function() {
    var k;
    k = "list";
    if (this.props['data-source'] === 'default') {
      k += " default";
    }
    if (this.props.dataType === 'post') {
      k += " posts";
    }
    return ul({
      className: k,
      key: "list-" + this.state.path
    }, this.renderList());
  }
});



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":14,"./LoadComponent.coffee":7}],7:[function(require,module,exports){
var div, input, recl, ref, textarea;

recl = React.createClass;

ref = [React.DOM.div, React.DOM.input, React.DOM.textarea], div = ref[0], input = ref[1], textarea = ref[2];

module.exports = recl({
  displayName: "Load",
  getInitialState: function() {
    return {
      anim: 0
    };
  },
  componentDidMount: function() {
    return this.interval = setInterval(this.setAnim, 100);
  },
  componentWillUnmount: function() {
    return clearInterval(this.interval);
  },
  setAnim: function() {
    var anim;
    anim = this.state.anim + 1;
    if (anim > 3) {
      anim = 0;
    }
    return this.setState({
      anim: anim
    });
  },
  render: function() {
    return div({
      className: "spin state-" + this.state.anim
    }, "");
  }
});



},{}],8:[function(require,module,exports){
var Dispatcher;

Dispatcher = require('flux').Dispatcher;

module.exports = _.extend(new Dispatcher(), {
  handleServerAction: function(action) {
    return this.dispatch({
      source: 'server',
      action: action
    });
  },
  handleViewAction: function(action) {
    return this.dispatch({
      source: 'view',
      action: action
    });
  }
});



},{"flux":10}],9:[function(require,module,exports){
var rend;

rend = React.render;

$(function() {
  var $body, TreeActions, TreePersistence, body, checkMove, checkScroll, codemirror, components, frag, head, kids, list, lost, po, setSo, so;
  $body = $('body');
  React.initializeTouchEvents(true);
  codemirror = React.createFactory(require('./components/CodeMirror.coffee'));
  head = React.createFactory(require('./components/AnchorComponent.coffee'));
  body = React.createFactory(require('./components/BodyComponent.coffee'));
  list = React.createFactory(require('./components/ListComponent.coffee'));
  kids = React.createFactory(require('./components/KidsComponent.coffee'));
  lost = React.createClass({
    render: function() {
      return React.DOM.div({}, "lost");
    }
  });
  components = {
    kids: kids,
    list: list,
    lost: lost,
    codemirror: codemirror
  };
  window.tree._basepath = window.urb.util.basepath("/");
  window.tree._basepath += (window.location.pathname.replace(window.tree._basepath, "")).split("/")[0];
  window.tree.basepath = function(path) {
    var _path;
    if (path[0] !== "/") {
      path = "/" + path;
    }
    _path = window.tree._basepath + path;
    if (_path.slice(-1) === "/") {
      _path = _path.slice(0, -1);
    }
    return _path;
  };
  window.tree.fragpath = function(path) {
    return path.replace(window.tree._basepath, "");
  };
  window.tree.reactify = function(obj) {
    var ref, ref1;
    switch (false) {
      case typeof obj !== "string":
        return obj;
      case obj.gn == null:
        return React.createElement((ref = components[obj.gn]) != null ? ref : obj.gn, (ref1 = obj.ga) != null ? ref1 : {}, obj.c.map(window.tree.reactify));
      default:
        throw "Bad react-json " + (JSON.stringify(obj));
    }
  };
  TreeActions = require('./actions/TreeActions.coffee');
  TreePersistence = require('./persistence/TreePersistence.coffee');
  frag = window.tree.fragpath(window.location.pathname);
  TreeActions.setCurr(frag);
  TreeActions.loadPath(frag, window.tree.body, window.tree.kids);
  rend(head({}, ""), $('#nav')[0]);
  rend(body({}, ""), $('#cont')[0]);
  checkScroll = function() {
    if ($(window).scrollTop() > 20) {
      return $('#nav').addClass('scrolling');
    } else {
      return $('#nav').removeClass('scrolling');
    }
  };
  setInterval(checkScroll, 500);
  po = {};
  po.cm = null;
  po.lm = null;
  po.cs = $(window).scrollTop();
  po.ls = $(window).scrollTop();
  $(document).mousemove(function(e) {
    return po.cm = {
      x: e.pageX,
      y: e.pageY
    };
  });
  checkMove = function() {
    var ds, dx, dy;
    if (po.lm !== null && po.cm !== null) {
      po.cs = $(window).scrollTop();
      ds = Math.abs(po.cs - po.ls);
      dx = Math.abs(po.cm.x - po.lm.x);
      dy = Math.abs(po.cm.y - po.lm.y);
      $('#nav').toggleClass('moving', dx > 20 || dy > 20);
    }
    po.lm = po.cm;
    return po.ls = po.cs;
  };
  setInterval(checkMove, 200);
  so = {};
  so.ls = $(window).scrollTop();
  so.cs = $(window).scrollTop();
  so.w = null;
  so.$n = $('#nav');
  so.$d = $('#nav > div');
  so.nh = $('#nav').outerHeight(true);
  setSo = function() {
    so.w = $(window).width();
    return so.$n = $('#nav');
  };
  setInterval(setSo, 200);
  $(window).on('resize', function(e) {
    if (so.w > 1170) {
      return so.$n.removeClass('m-up m-down m-fixed');
    }
  });
  return $(window).on('scroll', function(e) {
    var dy, sto, top;
    so.cs = $(window).scrollTop();
    if (so.w > 1170) {
      so.$n.removeClass('m-up m-down m-fixed');
    }
    if (so.w < 1170) {
      dy = so.ls - so.cs;
      so.$d.removeClass('focus');
      if (so.cs <= 0) {
        so.$n.removeClass('m-up');
        so.$n.addClass('m-down m-fixed');
        return;
      }
      if (so.$n.hasClass('m-fixed' && so.w < 1024)) {
        so.$n.css({
          left: -1 * $(window).scrollLeft()
        });
      }
      if (dy > 0) {
        if (!so.$n.hasClass('m-down')) {
          so.$n.removeClass('m-up').addClass('m-down');
          top = so.cs - so.nh;
          if (top < 0) {
            top = 0;
          }
          so.$n.offset({
            top: top
          });
        }
        if (so.$n.hasClass('m-down') && !so.$n.hasClass('m-fixed') && so.$n.offset().top >= so.cs) {
          so.$n.addClass('m-fixed');
          so.$n.attr({
            style: ''
          });
        }
      }
      if (dy < 0) {
        if (!so.$n.hasClass('m-up')) {
          so.$n.removeClass('m-down m-fixed').addClass('m-up');
          so.$n.attr({
            style: ''
          });
          top = so.cs;
          sto = so.$n.offset().top;
          if (top < 0) {
            top = 0;
          }
          if (top > sto && top < sto + so.nh) {
            top = sto;
          }
          so.$n.offset({
            top: top
          });
        }
      }
    }
    return so.ls = so.cs;
  });
});



},{"./actions/TreeActions.coffee":1,"./components/AnchorComponent.coffee":2,"./components/BodyComponent.coffee":3,"./components/CodeMirror.coffee":4,"./components/KidsComponent.coffee":5,"./components/ListComponent.coffee":6,"./persistence/TreePersistence.coffee":13}],10:[function(require,module,exports){
/**
 * Copyright (c) 2014-2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module.exports.Dispatcher = require('./lib/Dispatcher')

},{"./lib/Dispatcher":11}],11:[function(require,module,exports){
/*
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * @providesModule Dispatcher
 * @typechecks
 */

"use strict";

var invariant = require('./invariant');

var _lastID = 1;
var _prefix = 'ID_';

/**
 * Dispatcher is used to broadcast payloads to registered callbacks. This is
 * different from generic pub-sub systems in two ways:
 *
 *   1) Callbacks are not subscribed to particular events. Every payload is
 *      dispatched to every registered callback.
 *   2) Callbacks can be deferred in whole or part until other callbacks have
 *      been executed.
 *
 * For example, consider this hypothetical flight destination form, which
 * selects a default city when a country is selected:
 *
 *   var flightDispatcher = new Dispatcher();
 *
 *   // Keeps track of which country is selected
 *   var CountryStore = {country: null};
 *
 *   // Keeps track of which city is selected
 *   var CityStore = {city: null};
 *
 *   // Keeps track of the base flight price of the selected city
 *   var FlightPriceStore = {price: null}
 *
 * When a user changes the selected city, we dispatch the payload:
 *
 *   flightDispatcher.dispatch({
 *     actionType: 'city-update',
 *     selectedCity: 'paris'
 *   });
 *
 * This payload is digested by `CityStore`:
 *
 *   flightDispatcher.register(function(payload) {
 *     if (payload.actionType === 'city-update') {
 *       CityStore.city = payload.selectedCity;
 *     }
 *   });
 *
 * When the user selects a country, we dispatch the payload:
 *
 *   flightDispatcher.dispatch({
 *     actionType: 'country-update',
 *     selectedCountry: 'australia'
 *   });
 *
 * This payload is digested by both stores:
 *
 *    CountryStore.dispatchToken = flightDispatcher.register(function(payload) {
 *     if (payload.actionType === 'country-update') {
 *       CountryStore.country = payload.selectedCountry;
 *     }
 *   });
 *
 * When the callback to update `CountryStore` is registered, we save a reference
 * to the returned token. Using this token with `waitFor()`, we can guarantee
 * that `CountryStore` is updated before the callback that updates `CityStore`
 * needs to query its data.
 *
 *   CityStore.dispatchToken = flightDispatcher.register(function(payload) {
 *     if (payload.actionType === 'country-update') {
 *       // `CountryStore.country` may not be updated.
 *       flightDispatcher.waitFor([CountryStore.dispatchToken]);
 *       // `CountryStore.country` is now guaranteed to be updated.
 *
 *       // Select the default city for the new country
 *       CityStore.city = getDefaultCityForCountry(CountryStore.country);
 *     }
 *   });
 *
 * The usage of `waitFor()` can be chained, for example:
 *
 *   FlightPriceStore.dispatchToken =
 *     flightDispatcher.register(function(payload) {
 *       switch (payload.actionType) {
 *         case 'country-update':
 *           flightDispatcher.waitFor([CityStore.dispatchToken]);
 *           FlightPriceStore.price =
 *             getFlightPriceStore(CountryStore.country, CityStore.city);
 *           break;
 *
 *         case 'city-update':
 *           FlightPriceStore.price =
 *             FlightPriceStore(CountryStore.country, CityStore.city);
 *           break;
 *     }
 *   });
 *
 * The `country-update` payload will be guaranteed to invoke the stores'
 * registered callbacks in order: `CountryStore`, `CityStore`, then
 * `FlightPriceStore`.
 */

  function Dispatcher() {
    this.$Dispatcher_callbacks = {};
    this.$Dispatcher_isPending = {};
    this.$Dispatcher_isHandled = {};
    this.$Dispatcher_isDispatching = false;
    this.$Dispatcher_pendingPayload = null;
  }

  /**
   * Registers a callback to be invoked with every dispatched payload. Returns
   * a token that can be used with `waitFor()`.
   *
   * @param {function} callback
   * @return {string}
   */
  Dispatcher.prototype.register=function(callback) {
    var id = _prefix + _lastID++;
    this.$Dispatcher_callbacks[id] = callback;
    return id;
  };

  /**
   * Removes a callback based on its token.
   *
   * @param {string} id
   */
  Dispatcher.prototype.unregister=function(id) {
    invariant(
      this.$Dispatcher_callbacks[id],
      'Dispatcher.unregister(...): `%s` does not map to a registered callback.',
      id
    );
    delete this.$Dispatcher_callbacks[id];
  };

  /**
   * Waits for the callbacks specified to be invoked before continuing execution
   * of the current callback. This method should only be used by a callback in
   * response to a dispatched payload.
   *
   * @param {array<string>} ids
   */
  Dispatcher.prototype.waitFor=function(ids) {
    invariant(
      this.$Dispatcher_isDispatching,
      'Dispatcher.waitFor(...): Must be invoked while dispatching.'
    );
    for (var ii = 0; ii < ids.length; ii++) {
      var id = ids[ii];
      if (this.$Dispatcher_isPending[id]) {
        invariant(
          this.$Dispatcher_isHandled[id],
          'Dispatcher.waitFor(...): Circular dependency detected while ' +
          'waiting for `%s`.',
          id
        );
        continue;
      }
      invariant(
        this.$Dispatcher_callbacks[id],
        'Dispatcher.waitFor(...): `%s` does not map to a registered callback.',
        id
      );
      this.$Dispatcher_invokeCallback(id);
    }
  };

  /**
   * Dispatches a payload to all registered callbacks.
   *
   * @param {object} payload
   */
  Dispatcher.prototype.dispatch=function(payload) {
    invariant(
      !this.$Dispatcher_isDispatching,
      'Dispatch.dispatch(...): Cannot dispatch in the middle of a dispatch.'
    );
    this.$Dispatcher_startDispatching(payload);
    try {
      for (var id in this.$Dispatcher_callbacks) {
        if (this.$Dispatcher_isPending[id]) {
          continue;
        }
        this.$Dispatcher_invokeCallback(id);
      }
    } finally {
      this.$Dispatcher_stopDispatching();
    }
  };

  /**
   * Is this Dispatcher currently dispatching.
   *
   * @return {boolean}
   */
  Dispatcher.prototype.isDispatching=function() {
    return this.$Dispatcher_isDispatching;
  };

  /**
   * Call the callback stored with the given id. Also do some internal
   * bookkeeping.
   *
   * @param {string} id
   * @internal
   */
  Dispatcher.prototype.$Dispatcher_invokeCallback=function(id) {
    this.$Dispatcher_isPending[id] = true;
    this.$Dispatcher_callbacks[id](this.$Dispatcher_pendingPayload);
    this.$Dispatcher_isHandled[id] = true;
  };

  /**
   * Set up bookkeeping needed when dispatching.
   *
   * @param {object} payload
   * @internal
   */
  Dispatcher.prototype.$Dispatcher_startDispatching=function(payload) {
    for (var id in this.$Dispatcher_callbacks) {
      this.$Dispatcher_isPending[id] = false;
      this.$Dispatcher_isHandled[id] = false;
    }
    this.$Dispatcher_pendingPayload = payload;
    this.$Dispatcher_isDispatching = true;
  };

  /**
   * Clear bookkeeping used for dispatching.
   *
   * @internal
   */
  Dispatcher.prototype.$Dispatcher_stopDispatching=function() {
    this.$Dispatcher_pendingPayload = null;
    this.$Dispatcher_isDispatching = false;
  };


module.exports = Dispatcher;

},{"./invariant":12}],12:[function(require,module,exports){
/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * @providesModule invariant
 */

"use strict";

/**
 * Use invariant() to assert state which your program assumes to be true.
 *
 * Provide sprintf-style format (only %s is supported) and arguments
 * to provide information about what broke and what you were
 * expecting.
 *
 * The invariant message will be stripped in production, but the invariant
 * will remain to ensure logic does not differ in production.
 */

var invariant = function(condition, format, a, b, c, d, e, f) {
  if (false) {
    if (format === undefined) {
      throw new Error('invariant requires an error message argument');
    }
  }

  if (!condition) {
    var error;
    if (format === undefined) {
      error = new Error(
        'Minified exception occurred; use the non-minified dev environment ' +
        'for the full error message and additional helpful warnings.'
      );
    } else {
      var args = [a, b, c, d, e, f];
      var argIndex = 0;
      error = new Error(
        'Invariant Violation: ' +
        format.replace(/%s/g, function() { return args[argIndex++]; })
      );
    }

    error.framesToPop = 1; // we don't care about invariant's own frame
    throw error;
  }
};

module.exports = invariant;

},{}],13:[function(require,module,exports){
module.exports = {
  get: function(path, query, cb) {
    var url;
    url = (window.tree.basepath(path)) + ".json";
    if (query) {
      url += "?" + query;
    }
    return $.get(url, {}, function(data) {
      if (cb) {
        return cb(null, data);
      }
    });
  }
};



},{}],14:[function(require,module,exports){
var EventEmitter, MessageDispatcher, TreeStore, _cont, _curr, _snip, _tree, clog;

EventEmitter = require('events').EventEmitter;

MessageDispatcher = require('../dispatcher/Dispatcher.coffee');

clog = console.log;

_tree = {};

_cont = {};

_snip = {};

_curr = "";

TreeStore = _.extend(EventEmitter.prototype, {
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  removeChangeListener: function(cb) {
    return this.removeListener("change", cb);
  },
  emitChange: function() {
    return this.emit('change');
  },
  pathToArr: function(_path) {
    return _path.split("/");
  },
  getTree: function(_path) {
    var i, len, sub, tree;
    tree = _tree;
    for (i = 0, len = _path.length; i < len; i++) {
      sub = _path[i];
      tree = tree[sub];
      if (tree == null) {
        return null;
      }
    }
    return tree;
  },
  setCurr: function(path) {
    return _curr = path;
  },
  getCurr: function() {
    return _curr;
  },
  getCont: function() {
    return _cont;
  },
  mergePathToTree: function(path, kids) {
    var i, j, len, len1, ref, ref1, ref2, sub, tree, x;
    tree = _tree;
    ref = this.pathToArr(path);
    for (i = 0, len = ref.length; i < len; i++) {
      sub = ref[i];
      tree[sub] = (ref1 = tree[sub]) != null ? ref1 : {};
      tree = tree[sub];
    }
    for (j = 0, len1 = kids.length; j < len1; j++) {
      x = kids[j];
      tree[x] = (ref2 = tree[x]) != null ? ref2 : {};
    }
    return tree;
  },
  getSnip: function() {
    return _snip;
  },
  loadSnip: function(path, snip) {
    var k, results, v;
    this.mergePathToTree(path, _.pluck(snip, "name"));
    if ((snip != null ? snip.length : void 0) !== 0) {
      results = [];
      for (k in snip) {
        v = snip[k];
        results.push(_snip[path + "/" + v.name] = {
          head: window.tree.reactify(v.body.head),
          body: window.tree.reactify(v.body.body),
          orig: v.body
        });
      }
      return results;
    } else {
      return _cont[path] = window.tree.reactify({
        gn: 'div',
        c: [
          {
            gn: 'h1',
            ga: {
              className: 'error'
            },
            c: ['Error: Empty path']
          }, {
            gn: 'div',
            c: [
              {
                gn: 'pre',
                c: [this.getCurr()]
              }, {
                gn: 'span',
                c: ['is either empty or does not exist.']
              }
            ]
          }
        ]
      });
    }
  },
  loadKids: function(path, kids) {
    var k, results, v;
    this.mergePathToTree(path, _.pluck(kids, "name"));
    results = [];
    for (k in kids) {
      v = kids[k];
      results.push(_cont[path + "/" + v.name] = window.tree.reactify(v.body));
    }
    return results;
  },
  loadPath: function(path, body, kids) {
    this.mergePathToTree(path, kids);
    return _cont[path] = window.tree.reactify(body);
  },
  getKids: function() {
    return _.keys(this.getTree(_curr.split("/")));
  },
  getSiblings: function() {
    var curr;
    curr = _curr.split("/");
    curr.pop();
    if (curr.length !== 0) {
      return this.getTree(curr);
    } else {
      return {};
    }
  },
  getPrev: function() {
    var ind, key, par, sibs, win;
    sibs = _.keys(this.getSiblings()).sort();
    if (sibs.length < 2) {
      return null;
    } else {
      par = _curr.split("/");
      key = par.pop();
      ind = sibs.indexOf(key);
      win = ind - 1 >= 0 ? sibs[ind - 1] : sibs[sibs.length - 1];
      par.push(win);
      return par.join("/");
    }
  },
  getNext: function() {
    var ind, key, par, sibs, win;
    sibs = _.keys(this.getSiblings()).sort();
    if (sibs.length < 2) {
      return null;
    } else {
      par = _curr.split("/");
      key = par.pop();
      ind = sibs.indexOf(key);
      win = ind + 1 < sibs.length ? sibs[ind + 1] : sibs[0];
      par.push(win);
      return par.join("/");
    }
  },
  getPare: function() {
    var _path;
    _path = this.pathToArr(_curr);
    if (_path.length > 1) {
      _path.pop();
      _path = _path.join("/");
      if (_path === "") {
        _path = "/";
      }
      return _path;
    } else {
      return null;
    }
  },
  getCrumbs: function() {
    var _path, crum, crums, k, v;
    _path = this.pathToArr(_curr);
    crum = "";
    crums = [];
    for (k in _path) {
      v = _path[k];
      crum += "/" + v;
      crums.push({
        name: v,
        path: crum
      });
    }
    return crums;
  },
  getBody: function() {
    if (_cont[_curr]) {
      return _cont[_curr];
    } else {
      return null;
    }
  }
});

TreeStore.dispatchToken = MessageDispatcher.register(function(payload) {
  var action;
  action = payload.action;
  switch (action.type) {
    case 'path-load':
      TreeStore.loadPath(action.path, action.body, action.kids, action.snip);
      return TreeStore.emitChange();
    case 'snip-load':
      TreeStore.loadSnip(action.path, action.snip);
      return TreeStore.emitChange();
    case 'kids-load':
      TreeStore.loadKids(action.path, action.kids);
      return TreeStore.emitChange();
    case 'set-curr':
      TreeStore.setCurr(action.path);
      return TreeStore.emitChange();
  }
});

module.exports = TreeStore;



},{"../dispatcher/Dispatcher.coffee":8,"events":15}],15:[function(require,module,exports){
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
        len = arguments.length;
        args = new Array(len - 1);
        for (i = 1; i < len; i++)
          args[i - 1] = arguments[i];
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    len = arguments.length;
    args = new Array(len - 1);
    for (i = 1; i < len; i++)
      args[i - 1] = arguments[i];

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
    var m;
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
  } else {
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

EventEmitter.listenerCount = function(emitter, type) {
  var ret;
  if (!emitter._events || !emitter._events[type])
    ret = 0;
  else if (isFunction(emitter._events[type]))
    ret = 1;
  else
    ret = emitter._events[type].length;
  return ret;
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

},{}]},{},[9]);
