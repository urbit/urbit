(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee":[function(require,module,exports){
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
  setLoading: function(state) {
    return TreeDispatcher.handleViewAction({
      type: "set-load",
      load: state
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
  getPath: function(path, cb) {
    var query;
    query = null;
    if (typeof cb === 'string') {
      query = arguments[1];
      cb = arguments[2];
    }
    if (path.slice(-1) === "/") {
      path = path.slice(0, -1);
    }
    return TreePersistence.get(path, query, (function(_this) {
      return function(err, res) {
        switch (query) {
          case "snip":
            _this.loadSnip(path, res.snip);
            break;
          case "kids":
            _this.loadKids(path, res.kids);
            break;
          default:
            _this.loadPath(path, res.body, res.kids, res.snip);
        }
        if (cb) {
          return cb(err, res);
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



},{"../dispatcher/Dispatcher.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/dispatcher/Dispatcher.coffee","../persistence/TreePersistence.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/persistence/TreePersistence.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/AnchorComponent.coffee":[function(require,module,exports){
var BodyComponent, TreeActions, TreeStore, a, div, recl, ref;

BodyComponent = require('./BodyComponent.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a], div = ref[0], a = ref[1];

module.exports = recl({
  stateFromStore: function() {
    return {
      crum: TreeStore.getCrumbs(),
      curr: TreeStore.getCurr(),
      pare: TreeStore.getPare(),
      sibs: TreeStore.getSiblings(),
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
    var next, rend;
    if (hist !== false) {
      history.pushState({}, "", window.tree.basepath(href));
    }
    next = href.split("#")[0];
    rend = false;
    if (next !== this.state.curr) {
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
    var title;
    title = $('#cont h1').first().text();
    if (title.length === 0) {
      title = this.state.curr.split("/")[this.state.curr.split("/").length - 1];
    }
    return document.title = title + " - " + this.state.curr;
  },
  checkUp: function() {
    var up;
    up = this.state.curr.split("/");
    up.pop();
    up = up.join("/");
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
  render: function() {
    var _parts, ci, curr, href, k, obj, offset, p, parts, s, sibs, up;
    parts = [];
    if (this.state.pare) {
      href = window.tree.basepath(this.state.pare);
      parts.push(div({
        id: "up"
      }, a({
        key: "arow-up",
        href: href,
        className: "arow-up"
      }, "")));
      if (this.state.prev || this.state.next) {
        _parts = [];
        if (this.state.prev) {
          href = window.tree.basepath(this.state.prev);
          _parts.push(a({
            key: "arow-prev",
            href: href,
            className: "arow-prev"
          }, ""));
        }
        if (this.state.next) {
          href = window.tree.basepath(this.state.next);
          _parts.push(a({
            key: "arow-next",
            href: href,
            className: "arow-next"
          }, ""));
        }
        parts.push(div({
          id: "sides"
        }, _parts));
      }
    }
    curr = this.state.curr;
    if (_.keys(this.state.sibs).length > 0) {
      p = curr.split("/");
      curr = p.pop();
      up = p.join("/");
      ci = 0;
      k = 0;
      sibs = _.map(_.keys(this.state.sibs).sort(), (function(_this) {
        return function(i) {
          var c;
          c = "";
          if (curr === i) {
            c = "active";
            ci = k;
          }
          k++;
          href = window.tree.basepath(up + "/" + i);
          return div({
            className: c
          }, a({
            key: i + "-a",
            href: href,
            onClick: _this._click
          }, i));
        };
      })(this));
      offset = 0;
      if (ci > 0) {
        offset = 0;
      }
      s = {
        marginTop: ((ci * -24) - offset) + "px"
      };
      parts.push(div({
        key: "sibs",
        id: "sibs",
        style: s
      }, sibs));
    }
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
    return div(obj, parts);
  }
});



},{"../actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee","../stores/TreeStore.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/stores/TreeStore.coffee","./BodyComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/BodyComponent.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/BodyComponent.coffee":[function(require,module,exports){
var TreeActions, TreeStore, div, input, load, recl, ref, textarea;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

load = require('./LoadComponent.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.input, React.DOM.textarea], div = ref[0], input = ref[1], textarea = ref[2];

module.exports = recl({
  stateFromStore: function() {
    return {
      body: TreeStore.getBody(),
      load: TreeStore.getLoad(),
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
      TreeActions.setLoading(true);
      return TreeActions.getPath(path, (function(_this) {
        return function() {
          return TreeActions.setLoading(false);
        };
      })(this));
    }
  },
  render: function() {
    var parts, ref1;
    parts = [];
    parts.push(div({
      id: 'body',
      key: "body" + this.state.curr
    }, (ref1 = this.state.body) != null ? ref1 : div({
      className: "loading"
    }, load({}, ""))));
    return div({}, parts);
  }
});



},{"../actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee","../stores/TreeStore.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/stores/TreeStore.coffee","./LoadComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/LoadComponent.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/CodeMirror.coffee":[function(require,module,exports){
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



},{}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/KidsComponent.coffee":[function(require,module,exports){
var TreeActions, TreeStore, a, div, hr, li, recl, ref, ul;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a, React.DOM.ul, React.DOM.li, React.DOM.hr], div = ref[0], a = ref[1], ul = ref[2], li = ref[3], hr = ref[4];

module.exports = recl({
  stateFromStore: function() {
    var path, ref1;
    path = (ref1 = this.props.dataPath) != null ? ref1 : TreeStore.getCurr();
    return {
      cont: TreeStore.getCont(),
      tree: TreeStore.getTree(path.split("/")),
      path: path
    };
  },
  componentDidMount: function() {
    return TreeStore.addChangeListener(this._onChangeStore);
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  componentDidMount: function() {
    var cont, i, k, len, ref1;
    cont = true;
    ref1 = _.keys(this.state.tree);
    for (i = 0, len = ref1.length; i < len; i++) {
      k = ref1[i];
      if (!this.state.cont[this.state.path + "/" + k]) {
        cont = false;
      }
    }
    if (!this.state.tree || _.keys(this.state.tree).length === 0 || !cont) {
      return TreeActions.getPath(this.state.path, "kids");
    }
  },
  render: function() {
    var _list, doc, ref1;
    doc = (ref1 = this.state.tree) != null ? ref1 : [];
    _list = _.map(_.keys(doc).sort(), (function(_this) {
      return function(v) {
        var _path;
        _path = _this.state.path + "/" + v;
        return [
          div({
            key: "kid-" + v
          }, _this.state.cont[_path]), hr({}, "")
        ];
      };
    })(this));
    return div({
      key: "kids-" + this.state.path,
      className: "kids"
    }, _list);
  }
});



},{"../actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee","../stores/TreeStore.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/stores/TreeStore.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/ListComponent.coffee":[function(require,module,exports){
var TreeActions, TreeStore, a, div, h1, li, load, recl, ref, ul;

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

load = require('./LoadComponent.coffee');

recl = React.createClass;

ref = [React.DOM.div, React.DOM.a, React.DOM.ul, React.DOM.li, React.DOM.h1], div = ref[0], a = ref[1], ul = ref[2], li = ref[3], h1 = ref[4];

module.exports = recl({
  stateFromStore: function() {
    var path, ref1;
    path = (ref1 = this.props.dataPath) != null ? ref1 : TreeStore.getCurr();
    return {
      snip: TreeStore.getSnip(),
      tree: TreeStore.getTree(path.split("/")),
      path: path
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
  getCont: function() {
    var cont, i, k, keys, len;
    cont = true;
    keys = _.keys(this.state.tree);
    for (i = 0, len = keys.length; i < len; i++) {
      k = keys[i];
      if (!this.state.snip[this.state.path + "/" + k]) {
        cont = false;
      }
    }
    if (keys.length === 0) {
      cont = false;
    }
    return cont;
  },
  componentDidMount: function() {
    var cont;
    cont = this.getCont();
    TreeStore.addChangeListener(this._onChangeStore);
    if (!this.state.tree || _.keys(this.state.tree).length === 0 || !cont) {
      return TreeActions.getPath(this.state.path, "snip");
    }
  },
  render: function() {
    var _list, doc, k, ref1;
    doc = (ref1 = this.state.tree) != null ? ref1 : [];
    if (!this.getCont()) {
      _list = div({
        className: "loading"
      }, load({}, ""));
    } else {
      _list = _.map(_.keys(doc).sort(), (function(_this) {
        return function(v) {
          var _path, c, href, prev;
          _path = _this.state.path + "/" + v;
          if (_this.props.dataPreview != null) {
            c = "preview";
            if (_this.props.titlesOnly) {
              prev = _this.state.snip[_path].head;
            } else {
              prev = _this.state.snip[_path];
            }
          } else {
            c = "";
            prev = h1({}, v);
          }
          href = window.tree.basepath(_path);
          return li({}, a({
            href: href,
            className: c,
            key: "list-a-" + _path
          }, prev));
        };
      })(this));
    }
    k = "list";
    if (this.props['data-source'] === 'default') {
      k += " default";
    }
    return ul({
      className: k,
      key: "list-" + this.state.path
    }, _list);
  }
});



},{"../actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee","../stores/TreeStore.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/stores/TreeStore.coffee","./LoadComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/LoadComponent.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/LoadComponent.coffee":[function(require,module,exports){
var div, input, recl, ref, textarea;

recl = React.createClass;

ref = [React.DOM.div, React.DOM.input, React.DOM.textarea], div = ref[0], input = ref[1], textarea = ref[2];

module.exports = recl({
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



},{}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/dispatcher/Dispatcher.coffee":[function(require,module,exports){
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



},{"flux":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/index.js"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/main.coffee":[function(require,module,exports){
var rend;

rend = React.render;

$(function() {
  var $body, TreeActions, TreePersistence, body, checkMove, checkScroll, codemirror, frag, head, kids, list, lost, po, setSo, so;
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
  window.tree._basepath = window.location.pathname;
  window.tree._basepath = window.tree._basepath.split("/");
  window.tree._basepath = window.tree._basepath.slice(0, window.tree._basepath.indexOf("tree") + 1);
  window.tree._basepath = window.tree._basepath.join("/");
  window.tree.basepath = function(path) {
    if (path[0] !== "/") {
      path = "/" + path;
    }
    return window.tree._basepath + path;
  };
  window.tree.fragpath = function(path) {
    return path.replace(window.tree._basepath, "");
  };
  window.tree.init({
    kids: kids,
    list: list,
    lost: lost,
    codemirror: codemirror
  });
  window.tree.reactify = function(str) {
    return eval(str);
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



},{"./actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee","./components/AnchorComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/AnchorComponent.coffee","./components/BodyComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/BodyComponent.coffee","./components/CodeMirror.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/CodeMirror.coffee","./components/KidsComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/KidsComponent.coffee","./components/ListComponent.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/components/ListComponent.coffee","./persistence/TreePersistence.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/persistence/TreePersistence.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/index.js":[function(require,module,exports){
/**
 * Copyright (c) 2014-2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module.exports.Dispatcher = require('./lib/Dispatcher')

},{"./lib/Dispatcher":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/lib/Dispatcher.js"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/lib/Dispatcher.js":[function(require,module,exports){
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

},{"./invariant":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/lib/invariant.js"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/node_modules/flux/lib/invariant.js":[function(require,module,exports){
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

},{}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/persistence/TreePersistence.coffee":[function(require,module,exports){
var TreeActions;

TreeActions = require('../actions/TreeActions.coffee');

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



},{"../actions/TreeActions.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/actions/TreeActions.coffee"}],"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/stores/TreeStore.coffee":[function(require,module,exports){
var EventEmitter, MessageDispatcher, TreeStore, _cont, _curr, _load, _snip, _tree;

EventEmitter = require('events').EventEmitter;

MessageDispatcher = require('../dispatcher/Dispatcher.coffee');

_tree = {};

_cont = {};

_snip = {};

_load = false;

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
  pathToObj: function(_path, _obj, kids) {
    var __path, i, j, l, ref, ref1, results;
    __path = this.pathToArr(_path);
    for (i = j = 0, ref = __path.length - 1; 0 <= ref ? j <= ref : j >= ref; i = 0 <= ref ? ++j : --j) {
      _obj = _obj[__path[i]] = {};
    }
    if ((kids != null ? kids.length : void 0) > 0) {
      results = [];
      for (i = l = 0, ref1 = kids.length - 1; 0 <= ref1 ? l <= ref1 : l >= ref1; i = 0 <= ref1 ? ++l : --l) {
        results.push(_obj[kids[i]] = {});
      }
      return results;
    }
  },
  getTree: function(_path) {
    var i, j, ref, tree;
    tree = _tree;
    if (_path.length > 0) {
      for (i = j = 0, ref = _path.length - 1; 0 <= ref ? j <= ref : j >= ref; i = 0 <= ref ? ++j : --j) {
        if (tree[_path[i]]) {
          tree = tree[_path[i]];
        } else {
          return null;
        }
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
  setLoad: function(load) {
    return _load = load;
  },
  getLoad: function() {
    return _load;
  },
  mergePathToTree: function(path, kids) {
    var _obj;
    _obj = {};
    this.pathToObj(path, _obj, kids);
    return _.merge(_tree, _obj);
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
          body: window.tree.reactify(v.body.body)
        });
      }
      return results;
    } else {
      return _cont[path] = window.tree.reactify("React.createElement ('div', {}, [ React.createElement('h1', {className:'error'}, 'Error: Empty path'), React.createElement('div', {}, [ React.createElement('pre', {}, '" + (this.getCurr()) + "'), React.createElement('span', {}, 'is either empty or does not exist.') ]) ])");
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
    case 'set-load':
      TreeStore.setLoad(action.load);
      return TreeStore.emitChange();
  }
});

module.exports = TreeStore;



},{"../dispatcher/Dispatcher.coffee":"/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/dispatcher/Dispatcher.coffee","events":"/usr/local/lib/node_modules/watchify/node_modules/browserify/node_modules/events/events.js"}],"/usr/local/lib/node_modules/watchify/node_modules/browserify/node_modules/events/events.js":[function(require,module,exports){
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
        throw TypeError('Uncaught, unspecified "error" event.');
      }
      return false;
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

},{}]},{},["/Users/galen/src/urbit-dev/urb/zod/base/pub/tree/src/js/main.coffee"]);
