(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var TreeDispatcher, TreePersistence;

TreeDispatcher = require('../dispatcher/Dispatcher.coffee');

TreePersistence = require('../persistence/TreePersistence.coffee');

module.exports = {
  loadPath: function(path, data) {
    return TreeDispatcher.handleServerAction({
      path: path,
      data: data,
      type: "loadPath"
    });
  },
  clearData: function() {
    TreePersistence.refresh();
    return TreeDispatcher.handleServerAction({
      type: "clearData"
    });
  },
  sendQuery: function(path, query) {
    if (query == null) {
      return;
    }
    if (path.slice(-1) === "/") {
      path = path.slice(0, -1);
    }
    return TreePersistence.get(path, query, (function(_this) {
      return function(err, res) {
        if (err != null) {
          throw err;
        }
        return _this.loadPath(path, res);
      };
    })(this));
  },
  registerComponent: function(name, comp) {
    var obj;
    return this.addVirtual((
      obj = {},
      obj["" + name] = comp,
      obj
    ));
  },
  addVirtual: function(components) {
    return TreeDispatcher.handleViewAction({
      type: "addVirtual",
      components: components
    });
  },
  addComment: function(path, text) {
    if (path[0] !== "/") {
      path = "/" + path;
    }
    return TreePersistence.put("write-comment", path, text);
  },
  setCurr: function(path) {
    return TreeDispatcher.handleViewAction({
      type: "setCurr",
      path: path
    });
  },
  setNav: function(arg) {
    var dpad, sibs, subnav, title;
    title = arg.title, dpad = arg.dpad, sibs = arg.sibs, subnav = arg.subnav;
    return TreeDispatcher.handleViewAction({
      title: title,
      dpad: dpad,
      sibs: sibs,
      subnav: subnav,
      type: "setNav"
    });
  },
  toggleNav: function() {
    return TreeDispatcher.handleViewAction({
      type: "toggleNav"
    });
  },
  clearNav: function() {
    return TreeDispatcher.handleViewAction({
      type: "clearNav"
    });
  }
};


},{"../dispatcher/Dispatcher.coffee":18,"../persistence/TreePersistence.coffee":20}],2:[function(require,module,exports){
var BodyComponent, Dpad, Nav, Sibs, TreeActions, TreeStore, a, button, clas, div, li, query, reactify, recl, ref, rend, ul, util;

clas = require('classnames');

BodyComponent = React.createFactory(require('./BodyComponent.coffee'));

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

Sibs = require('./SibsComponent.coffee');

Dpad = require('./DpadComponent.coffee');

util = require('../utils/util.coffee');

recl = React.createClass;

rend = ReactDOM.render;

ref = React.DOM, div = ref.div, a = ref.a, ul = ref.ul, li = ref.li, button = ref.button;

Nav = React.createFactory(query({
  path: 't',
  kids: {
    name: 't',
    head: 'r',
    meta: 'j'
  }
}, recl({
  displayName: "Links",
  stateFromStore: function() {
    return TreeStore.getNav();
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    if (this.isMounted()) {
      return this.setState(this.stateFromStore());
    }
  },
  componentDidMount: function() {
    return TreeStore.addChangeListener(this._onChangeStore);
  },
  componentWillUnmount: function() {
    return TreeStore.removeChangeListener(this._onChangeStore);
  },
  onClick: function() {
    return this.toggleFocus();
  },
  onMouseOver: function() {
    return this.toggleFocus(true);
  },
  onMouseOut: function() {
    return this.toggleFocus(false);
  },
  onTouchStart: function() {
    return this.ts = Number(Date.now());
  },
  onTouchEnd: function() {
    var dt;
    return dt = this.ts - Number(Date.now());
  },
  _home: function() {
    return this.props.goTo("/");
  },
  toggleFocus: function(state) {
    return $(ReactDOM.findDOMNode(this)).toggleClass('focus', state);
  },
  toggleNav: function() {
    return TreeActions.toggleNav();
  },
  render: function() {
    var attr, dpad, navClas, sibs, title, toggleClas;
    attr = {
      onMouseOver: this.onMouseOver,
      onMouseOut: this.onMouseOut,
      onClick: this.onClick,
      onTouchStart: this.onTouchStart,
      onTouchEnd: this.onTouchEnd
    };
    if (_.keys(window).indexOf("ontouchstart") !== -1) {
      delete attr.onMouseOver;
      delete attr.onMouseOut;
    }
    navClas = clas({
      'col-md-2': true,
      ctrl: true,
      open: this.state.open === true
    });
    attr = _.extend(attr, {
      className: navClas,
      key: "nav"
    });
    title = this.state.title ? this.state.title : "";
    dpad = this.state.dpad !== false ? Dpad(this.props, "") : "";
    sibs = this.state.sibs !== false ? Sibs(_.merge(this.props, {
      toggleNav: this.toggleNav
    }), "") : "";
    toggleClas = clas({
      'navbar-toggler': true,
      show: this.state.subnav != null
    });
    return div(attr, div({
      className: 'links',
      key: "links"
    }, div({
      className: 'icon'
    }, div({
      className: 'home',
      onClick: this._home
    }, ""), div({
      className: 'app'
    }, title), dpad, button({
      className: toggleClas,
      type: 'button',
      onClick: this.toggleNav
    }, "☰")), sibs));
  }
}), recl({
  displayName: "Links_loading",
  _home: function() {
    return this.props.goTo("/");
  },
  render: function() {
    return div({
      className: "col-md-2 ctrl",
      key: "nav-loading"
    }, div({
      className: 'links'
    }, div({
      className: 'icon'
    }, div({
      className: 'home',
      onClick: this._home
    }, "")), ul({
      className: "nav"
    }, li({
      className: "nav-item selected"
    }, a({
      className: "nav-link"
    }, this.props.curr)))));
  }
})));

module.exports = query({
  sein: 't',
  path: 't',
  name: 't',
  meta: 'j'
}, recl({
  displayName: "Anchor",
  stateFromStore: function() {
    return TreeStore.getNav();
  },
  getInitialState: function() {
    return _.extend(this.stateFromStore(), {
      url: window.location.pathname
    });
  },
  _onChangeStore: function() {
    if (this.isMounted()) {
      return this.setState(this.stateFromStore());
    }
  },
  componentWillUnmount: function() {
    clearInterval(this.interval);
    $('body').off('click', 'a');
    return TreeStore.removeChangeListener(this._onChangeStore);
  },
  componentDidUpdate: function() {
    return this.setTitle();
  },
  componentDidMount: function() {
    var _this;
    this.setTitle();
    this.interval = setInterval(this.checkURL, 100);
    TreeStore.addChangeListener(this._onChangeStore);
    _this = this;
    return $('body').on('click', 'a', function(e) {
      var href;
      href = $(this).attr('href');
      if (href && !/^https?:\/\//i.test(href)) {
        e.preventDefault();
        if ((href != null ? href[0] : void 0) !== "/") {
          href = (document.location.pathname.replace(/[^\/]*\/?$/, '')) + href;
        }
        return _this.goTo(util.fragpath(href));
      }
    });
  },
  setTitle: function() {
    var ref1, title;
    title = $('#body h1').first().text() || this.props.name;
    if ((ref1 = this.props.meta) != null ? ref1.title : void 0) {
      title = this.props.meta.title;
    }
    return document.title = title + " - " + this.props.path;
  },
  setPath: function(href, hist) {
    var href_parts, next;
    href_parts = href.split("#");
    next = href_parts[0];
    if (next.substr(-1) === "/") {
      next = next.slice(0, -1);
    }
    href_parts[0] = next;
    if (hist !== false) {
      history.pushState({}, "", util.basepath(href_parts.join("")));
    }
    if (next !== this.props.path) {
      React.unmountComponentAtNode($('#body')[0]);
      TreeActions.setCurr(next);
      return rend(BodyComponent({}, ""), $('#body')[0]);
    }
  },
  reset: function() {
    $("html,body").animate({
      scrollTop: 0
    });
    $('#nav').attr('style', '');
    $('#nav').removeClass('scrolling m-up');
    return $('#nav').addClass('m-down m-fixed');
  },
  goTo: function(path) {
    this.reset();
    return this.setPath(path);
  },
  checkURL: function() {
    if (this.state.url !== window.location.pathname) {
      this.reset();
      this.setPath(util.fragpath(window.location.pathname), false);
      return this.setState({
        url: window.location.pathname
      });
    }
  },
  render: function() {
    var kids;
    if (this.props.meta.anchor === 'none') {
      return div({}, "");
    }
    kids = [
      Nav({
        curr: this.props.name,
        dataPath: this.props.sein,
        sein: this.props.sein,
        goTo: this.goTo,
        key: "nav"
      }, "div")
    ];
    if (this.state.subnav) {
      kids.push(reactify({
        gn: this.state.subnav,
        ga: {
          open: this.state.open,
          toggle: TreeActions.toggleNav
        },
        c: []
      }, "subnav"));
    }
    return div({}, kids);
  }
}));


},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":21,"../utils/util.coffee":23,"./Async.coffee":3,"./BodyComponent.coffee":4,"./DpadComponent.coffee":7,"./Reactify.coffee":13,"./SibsComponent.coffee":16,"classnames":24}],3:[function(require,module,exports){
var TreeActions, TreeStore, _load, code, div, recl, ref, span;

_load = require('./LoadComponent.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, span = ref.span, code = ref.code;

module.exports = function(queries, Child, load) {
  if (load == null) {
    load = _load;
  }
  return recl({
    displayName: "Async",
    getInitialState: function() {
      return this.stateFromStore();
    },
    _onChangeStore: function() {
      if (this.isMounted()) {
        return this.setState(this.stateFromStore());
      }
    },
    getPath: function() {
      var path, ref1;
      path = (ref1 = this.props.dataPath) != null ? ref1 : TreeStore.getCurr();
      if (path.slice(-1) === "/") {
        return path.slice(0, -1);
      } else {
        return path;
      }
    },
    stateFromStore: function() {
      var fresh, ref1;
      fresh = TreeStore.fulfill(this.getPath(), queries);
      return {
        fresh: fresh,
        got: _.merge({}, (ref1 = this.state) != null ? ref1.got : void 0, fresh)
      };
    },
    componentDidMount: function() {
      TreeStore.addChangeListener(this._onChangeStore);
      return this.checkPath();
    },
    componentWillUnmount: function() {
      return TreeStore.removeChangeListener(this._onChangeStore);
    },
    componentDidUpdate: function(_props, _state) {
      if (_props !== this.props) {
        this.setState(this.stateFromStore());
      }
      return this.checkPath();
    },
    checkPath: function() {
      return TreeActions.sendQuery(this.getPath(), this.filterFreshQueries());
    },
    filterFreshQueries: function() {
      return this.filterWith(this.state.fresh, queries);
    },
    filterQueries: function() {
      return this.filterWith(this.state.got, queries);
    },
    filterWith: function(have, _queries) {
      var k, kid, ref1, request;
      if (have == null) {
        return _queries;
      }
      request = {};
      for (k in _queries) {
        if (k !== 'kids') {
          if (have[k] === void 0) {
            request[k] = _queries[k];
          }
        }
      }
      if (_queries.kids != null) {
        if (have.kids == null) {
          request.kids = _queries.kids;
        } else {
          request.kids = {};
          ref1 = have.kids;
          for (k in ref1) {
            kid = ref1[k];
            _.merge(request.kids, this.filterWith(kid, _queries.kids));
          }
          if (_.isEmpty(request.kids)) {
            delete request.kids;
          }
        }
      }
      if (!_.isEmpty(request)) {
        return request;
      }
    },
    scrollHash: function() {
      var ref1;
      return (ref1 = this.getHashElement()) != null ? ref1.scrollIntoView() : void 0;
    },
    getHashElement: function() {
      var hash;
      hash = document.location.hash;
      if (hash) {
        return document.getElementById(hash.slice(1));
      }
    },
    render: function() {
      return div({}, this.filterQueries() != null ? React.createElement(load, this.props) : (!this.getHashElement() ? setTimeout(this.scrollHash, 0) : void 0, React.createElement(Child, _.extend({}, this.props, this.state.got), this.props.children)));
    }
  });
};


},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":21,"./LoadComponent.coffee":11}],4:[function(require,module,exports){
var Comment, TreeActions, a, clas, div, extras, img, input, load, p, query, reactify, recl, ref, rele, util;

clas = require('classnames');

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeActions = require('../actions/TreeActions.coffee');

util = require('../utils/util.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, p = ref.p, img = ref.img, a = ref.a, input = ref.input;

Comment = function(arg) {
  var body, time;
  time = arg.time, body = arg.body;
  return div({}, "" + (new Date(time)), reactify(body));
};

extras = {
  spam: recl({
    displayName: "Spam",
    render: function() {
      if (document.location.hostname !== 'urbit.org') {
        return div({});
      }
      return div({
        className: 'spam'
      }, a({
        href: "http://urbit.org#sign-up"
      }, "Sign up"), " for our newsletter.");
    }
  }),
  logo: recl({
    displayName: "Logo",
    render: function() {
      var color, src;
      color = this.props.color;
      if (color === "white" || color === "black") {
        src = "//storage.googleapis.com/urbit-extra/logo/logo-" + color + "-100x100.png";
      }
      return a({
        href: "http://urbit.org",
        style: {
          border: "none"
        }
      }, img({
        src: src,
        className: "logo first"
      }));
    }
  }),
  next: query({
    path: 't',
    kids: {
      name: 't',
      head: 'r',
      meta: 'j'
    }
  }, recl({
    displayName: "Next",
    render: function() {
      var curr, index, keys, next, ref1;
      curr = this.props.kids[this.props.curr];
      if (curr != null ? (ref1 = curr.meta) != null ? ref1.next : void 0 : void 0) {
        keys = util.getKeys(this.props.kids);
        if (keys.length > 1) {
          index = keys.indexOf(this.props.curr);
          next = index + 1;
          if (next === keys.length) {
            next = 0;
          }
          next = keys[next];
          next = this.props.kids[next];
          if (next) {
            return div({
              className: "link-next"
            }, a({
              href: this.props.path + "/" + next.name
            }, "Next: " + next.meta.title));
          }
        }
      }
      return div({}, "");
    }
  })),
  comments: query({
    comt: 'j',
    path: 't'
  }, recl({
    displayName: "Comments",
    getInitialState: function() {
      return {
        loading: false
      };
    },
    componentDidUpdate: function(_props) {
      if (this.props.comt.length > _props.comt.length) {
        return this.setState({
          loading: false
        });
      }
    },
    onKeyDown: function(e) {
      if ("Enter" === e.key) {
        this.setState({
          loading: true
        });
        return TreeActions.addComment(this.props.path, this.refs["in"].value);
      }
    },
    render: function() {
      return div({}, "Add comment:", (this.state.loading ? rele(load) : input({
        className: "comment",
        type: "text",
        ref: "in",
        onKeyDown: this.onKeyDown
      })), this.props.comt.map(function(props, key) {
        return rele(Comment, _.extend({
          key: key
        }, props));
      }));
    }
  })),
  footer: recl({
    displayName: "Footer",
    render: function() {
      return div({
        className: "footer"
      }, p({}, "This page was served by Urbit."));
    }
  })
};

module.exports = query({
  body: 'r',
  name: 't',
  path: 't',
  meta: 'j',
  sein: 't'
}, recl({
  displayName: "Body",
  render: function() {
    var bodyClas, containerClas, extra, ref1;
    extra = (function(_this) {
      return function(name, props) {
        if (props == null) {
          props = {};
        }
        if (_this.props.meta[name] != null) {
          return React.createElement(extras[name], props);
        }
      };
    })(this);
    containerClas = clas({
      "col-md-10": true,
      "col-md-offset-2": this.props.meta.anchor !== 'none',
      body: true
    });
    bodyClas = clas((ref1 = this.props.meta.layout) != null ? ref1.split(',') : void 0);
    return div({
      className: containerClas
    }, [
      div({
        key: "body" + this.props.path,
        bodyClas: bodyClas
      }, extra('spam'), extra('logo', {
        color: this.props.meta.logo
      }), reactify(this.props.body), extra('next', {
        dataPath: this.props.sein,
        curr: this.props.name
      }), extra('comments'), extra('footer'))
    ]);
  }
}));


},{"../actions/TreeActions.coffee":1,"../utils/util.coffee":23,"./Async.coffee":3,"./LoadComponent.coffee":11,"./Reactify.coffee":13,"classnames":24}],5:[function(require,module,exports){
var div, recl, ref, textarea;

recl = React.createClass;

ref = React.DOM, div = ref.div, textarea = ref.textarea;

module.exports = recl({
  render: function() {
    return div({}, textarea({
      ref: 'ed',
      value: this.props.value
    }));
  },
  componentDidMount: function() {
    return CodeMirror.fromTextArea(ReactDOM.findDOMNode(this.refs.ed), {
      readOnly: true,
      lineNumbers: true
    });
  }
});


},{}],6:[function(require,module,exports){
var div, recl;

recl = React.createClass;

div = React.DOM.div;

module.exports = {
  codemirror: require('./CodeMirror.coffee'),
  search: require('./SearchComponent.coffee'),
  list: require('./ListComponent.coffee'),
  kids: require('./KidsComponent.coffee'),
  toc: require('./TocComponent.coffee'),
  email: require('./EmailComponent.coffee'),
  module: require('./ModuleComponent.coffee'),
  script: require('./ScriptComponent.coffee'),
  lost: recl({
    render: function() {
      return div({}, "<lost(", this.props.children, ")>");
    }
  })
};


},{"./CodeMirror.coffee":5,"./EmailComponent.coffee":8,"./KidsComponent.coffee":9,"./ListComponent.coffee":10,"./ModuleComponent.coffee":12,"./ScriptComponent.coffee":14,"./SearchComponent.coffee":15,"./TocComponent.coffee":17}],7:[function(require,module,exports){
var a, div, recl, ref, util;

util = require('../utils/util.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, a = ref.a;

module.exports = React.createFactory(recl({
  displayName: "Dpad",
  renderUp: function() {
    if (this.props.sein) {
      return this.renderArrow("up", this.props.sein);
    }
  },
  renderArrow: function(name, path) {
    var href;
    href = util.basepath(path);
    return a({
      href: href,
      key: "" + name,
      className: "" + name
    }, "");
  },
  renderArrows: function() {
    var index, keys, next, prev, sein;
    keys = util.getKeys(this.props.kids);
    if (keys.length > 1) {
      index = keys.indexOf(this.props.curr);
      prev = index - 1;
      next = index + 1;
      if (prev < 0) {
        prev = keys.length - 1;
      }
      if (next === keys.length) {
        next = 0;
      }
      prev = keys[prev];
      next = keys[next];
    }
    if (this.props.sein) {
      sein = this.props.sein;
      if (sein === "/") {
        sein = "";
      }
      return div({}, prev ? this.renderArrow("prev", sein + "/" + prev) : void 0, next ? this.renderArrow("next", sein + "/" + next) : void 0);
    }
  },
  render: function() {
    return div({
      className: 'dpad',
      key: 'dpad'
    }, this.renderUp(), this.renderArrows());
  }
}));


},{"../utils/util.coffee":23}],8:[function(require,module,exports){
var button, div, input, p, reactify, recl, ref;

reactify = require('./Reactify.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, p = ref.p, button = ref.button, input = ref.input;

module.exports = recl({
  displayName: "email",
  getInitialState: function() {
    return {
      submit: false,
      email: ""
    };
  },
  onClick: function() {
    return this.submit();
  },
  onKeyUp: function(e) {
    var email, valid;
    email = this.$email.val();
    valid = email.indexOf('@') !== -1 && email.indexOf('.') !== -1 && email.length > 7 && email.split(".")[1].length > 1 && email.split("@")[0].length > 0 && email.split("@")[1].length > 4;
    this.$email.toggleClass('valid', valid);
    this.$email.removeClass('error');
    if (e.keyCode === 13) {
      if (valid === true) {
        this.submit();
        e.stopPropagation();
        e.preventDefault();
        return false;
      } else {
        return this.$email.addClass('error');
      }
    }
  },
  submit: function() {
    return $.post(this.props.dataPath, {
      email: this.$email.val()
    }, (function(_this) {
      return function() {
        return _this.setState({
          submit: true
        });
      };
    })(this));
  },
  componentDidMount: function() {
    return this.$email = $('input.email');
  },
  render: function() {
    var cont;
    if (this.state.submit === false) {
      cont = [
        input({
          key: "field",
          className: "email",
          placeholder: "your@email.com",
          onKeyUp: this.onKeyUp
        }, this.state.email), button({
          key: "submit",
          className: "submit",
          onClick: this.onClick
        }, "Sign up")
      ];
    } else {
      cont = [
        div({
          className: "submitted"
        }, "Got it. Thanks!")
      ];
    }
    return p({
      className: "email",
      id: "sign-up"
    }, cont);
  }
});


},{"./Reactify.coffee":13}],9:[function(require,module,exports){
var a, div, hr, li, query, reactify, recl, ref, ul;

reactify = require('./Reactify.coffee');

query = require('./Async.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, a = ref.a, ul = ref.ul, li = ref.li, hr = ref.hr;

module.exports = query({
  kids: {
    body: 'r',
    meta: 'j'
  }
}, recl({
  displayName: "Kids",
  render: function() {
    var _k, d, elem, k, keyed, keys, klass, ref1, ref2, ref3, ref4, sorted, str, v;
    klass = "kids";
    if (this.props.dataType) {
      klass += " " + this.props.dataType;
    }
    sorted = true;
    keyed = {};
    ref1 = this.props.kids;
    for (k in ref1) {
      v = ref1[k];
      if (this.props.sortBy) {
        if (this.props.sortBy === 'date') {
          if (((ref2 = v.meta) != null ? ref2.date : void 0) == null) {
            sorted = false;
            continue;
          }
          d = v.meta.date.slice(1).split(".");
          if (d.length < 3) {
            sorted = false;
            continue;
          }
          str = d[0] + "-" + d[1] + "-" + d[2];
          if (d.length > 3) {
            str += " " + d[3] + ":" + d[4] + ":" + d[5];
          }
          _k = Number(new Date(str));
          keyed[_k] = k;
        }
      } else {
        if (((ref3 = v.meta) != null ? ref3.sort : void 0) == null) {
          sorted = false;
        }
        keyed[Number((ref4 = v.meta) != null ? ref4.sort : void 0)] = k;
      }
    }
    if (sorted === false) {
      keyed = _.keys(this.props.kids);
    }
    keys = _.keys(keyed).sort();
    if (this.props.sortBy === 'date') {
      keys.reverse();
    }
    return div({
      className: klass
    }, (function() {
      var i, len, ref5, results;
      results = [];
      for (i = 0, len = keys.length; i < len; i++) {
        k = keys[i];
        elem = (ref5 = this.props.kids[keyed[k]]) != null ? ref5 : "";
        results.push([
          div({
            key: keyed[k]
          }, reactify(elem.body)), hr({}, "")
        ]);
      }
      return results;
    }).call(this));
  }
}));


},{"./Async.coffee":3,"./Reactify.coffee":13}],10:[function(require,module,exports){
var a, clas, div, h1, li, pre, query, reactify, recl, ref, span, ul, util;

clas = require('classnames');

reactify = require('./Reactify.coffee');

query = require('./Async.coffee');

util = require('../utils/util.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, pre = ref.pre, span = ref.span, a = ref.a, ul = ref.ul, li = ref.li, h1 = ref.h1;

module.exports = query({
  path: 't',
  kids: {
    snip: 'r',
    head: 'r',
    meta: 'j'
  }
}, recl({
  displayName: "List",
  render: function() {
    var k, kids;
    k = clas({
      list: true
    }, this.props.dataType, {
      posts: this.props.dataType === 'post',
      "default": this.props['data-source'] === 'default'
    }, this.props.className);
    kids = this.renderList();
    if (!(kids.length === 0 && (this.props.is404 != null))) {
      return ul({
        className: k
      }, kids);
    }
    return div({
      className: k
    }, h1({
      className: 'red inverse block error'
    }, 'Error: Empty path'), div({}, pre({}, this.props.path), span({}, 'is either empty or does not exist.')));
  },
  renderList: function() {
    var _date, _k, _keys, date, elem, href, i, item, k, len, parts, path, preview, ref1, ref2, ref3, ref4, ref5, ref6, ref7, results, sorted, title, v;
    sorted = true;
    _keys = [];
    ref1 = this.props.kids;
    for (k in ref1) {
      v = ref1[k];
      if (this.props.sortBy) {
        if (this.props.sortBy === 'date') {
          if (((ref2 = v.meta) != null ? ref2.date : void 0) == null) {
            sorted = false;
          }
          _k = Number(v.meta.date.slice(1).replace(/\./g, ""));
          _keys[_k] = k;
        }
      } else {
        if (((ref3 = v.meta) != null ? ref3.sort : void 0) == null) {
          sorted = false;
        }
        _keys[Number((ref4 = v.meta) != null ? ref4.sort : void 0)] = k;
      }
    }
    if (this.props.sortBy === 'date') {
      _keys.reverse();
    }
    if (sorted !== true) {
      _keys = _.keys(this.props.kids).sort();
    }
    if (this.props.dataType === 'post') {
      _keys = _keys.reverse();
    }
    ref5 = _.values(_keys);
    results = [];
    for (i = 0, len = ref5.length; i < len; i++) {
      item = ref5[i];
      path = this.props.path + "/" + item;
      elem = this.props.kids[item];
      if (elem.meta.hide != null) {
        continue;
      }
      href = util.basepath(path);
      if (elem.meta.link) {
        href = elem.meta.link;
      }
      parts = [];
      title = null;
      if ((ref6 = elem.meta) != null ? ref6.title : void 0) {
        title = {
          gn: 'h1',
          c: [elem.meta.title]
        };
      }
      if (!title && elem.head.c.length > 0) {
        title = elem.head;
      }
      if (!title) {
        title = {
          gn: 'h1',
          c: [item]
        };
      }
      if (!this.props.titlesOnly) {
        if (this.props.dataDates) {
          _date = elem.meta.date;
          if (!_date || _date.length === 0) {
            _date = "";
          }
          date = {
            gn: 'div',
            ga: {
              className: 'date'
            },
            c: [_date]
          };
          parts.push(date);
        }
      }
      parts.push(title);
      if (!this.props.titlesOnly) {
        if (this.props.dataPreview) {
          if (this.props.dataType === 'post' && !elem.meta.preview) {
            parts.push.apply(parts, elem.snip.c.slice(0, 2));
          } else {
            if (elem.meta.preview) {
              preview = {
                gn: 'p',
                c: [elem.meta.preview]
              };
            } else {
              preview = elem.snip;
            }
            parts.push(preview);
          }
        }
      }
      results.push(li({
        key: item,
        className: (ref7 = this.props.dataType) != null ? ref7 : ""
      }, a({
        href: href,
        className: clas({
          preview: this.props.dataPreview != null
        })
      }, reactify({
        gn: 'div',
        c: parts
      }))));
    }
    return results;
  }
}));


},{"../utils/util.coffee":23,"./Async.coffee":3,"./Reactify.coffee":13,"classnames":24}],11:[function(require,module,exports){
var div, recl, ref, span;

recl = React.createClass;

ref = React.DOM, span = ref.span, div = ref.div;

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
    return span({
      className: "loading state-" + this.state.anim
    }, '');
  }
});


},{}],12:[function(require,module,exports){
var TreeActions, div, recl;

recl = React.createClass;

div = React.DOM.div;

TreeActions = require('../actions/TreeActions.coffee');

module.exports = recl({
  displayName: "Module",
  componentDidMount: function() {
    return setTimeout((function(_this) {
      return function() {
        return TreeActions.setNav({
          title: _this.props["nav:title"],
          dpad: _this.props["nav:no-dpad"] != null ? false : void 0,
          sibs: _this.props["nav:no-sibs"] != null ? false : void 0,
          subnav: _this.props["nav:subnav"]
        }, 0);
      };
    })(this));
  },
  componentWillUnmount: function() {
    return TreeActions.clearNav();
  },
  render: function() {
    return div({
      className: "module"
    }, this.props.children);
  }
});


},{"../actions/TreeActions.coffee":1}],13:[function(require,module,exports){
var TreeStore, Virtual, div, load, reactify, recl, ref, rele, span, walk;

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, span = ref.span;

load = React.createFactory(require('./LoadComponent.coffee'));

TreeStore = require('../stores/TreeStore.coffee');

walk = function(root, _nil, _str, _comp) {
  var _walk;
  _walk = function(elem, key) {
    var c, ga, gn, ref1;
    switch (false) {
      case !(elem == null):
        return _nil();
      case typeof elem !== "string":
        return _str(elem);
      case elem.gn == null:
        gn = elem.gn, ga = elem.ga, c = elem.c;
        c = (ref1 = c != null ? c.map(_walk) : void 0) != null ? ref1 : [];
        return _comp.call(elem, {
          gn: gn,
          ga: ga,
          c: c
        }, key);
      default:
        throw "Bad react-json " + (JSON.stringify(elem));
    }
  };
  return _walk(root);
};

Virtual = recl({
  displayName: "Virtual",
  getInitialState: function() {
    return this.stateFromStore();
  },
  stateFromStore: function() {
    return {
      components: TreeStore.getVirtualComponents()
    };
  },
  _onChangeStore: function() {
    if (this.isMounted()) {
      return this.setState(this.stateFromStore());
    }
  },
  componentDidMount: function() {
    return TreeStore.addChangeListener(this._onChangeStore);
  },
  componentWillUnmount: function() {
    return TreeStore.removeChangeListener(this._onChangeStore);
  },
  render: function() {
    var components;
    components = this.state.components;
    return walk(this.props.manx, function() {
      return load({}, "");
    }, function(str) {
      return str;
    }, function(arg, key) {
      var c, ga, gn, ref1;
      gn = arg.gn, ga = arg.ga, c = arg.c;
      return rele((ref1 = components[gn]) != null ? ref1 : gn, _.extend({
        key: key
      }, ga), c.length ? c : void 0);
    });
  }
});

reactify = function(manx, key) {
  return rele(Virtual, {
    manx: manx,
    key: key
  });
};

module.exports = _.extend(reactify, {
  walk: walk,
  Virtual: Virtual
});


},{"../stores/TreeStore.coffee":21,"./LoadComponent.coffee":11}],14:[function(require,module,exports){
var recl, rele;

recl = React.createClass;

rele = React.createElement;

module.exports = recl({
  displayName: "Script",
  componentDidMount: function() {
    var s;
    s = document.createElement('script');
    _.assign(s, this.props);
    urb.waspElem(s);
    document.body.appendChild(s);
    return this.js = s;
  },
  componentWillUnmount: function() {
    return document.body.removeChild(this.js);
  },
  render: function() {
    return rele("script", this.props);
  }
});


},{}],15:[function(require,module,exports){
var a, div, input, query, reactify, recl, ref,
  slice = [].slice;

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

recl = React.createClass;

ref = React.DOM, a = ref.a, div = ref.div, input = ref.input;

module.exports = query({
  name: 't',
  kids: {
    sect: 'j'
  }
}, recl({
  hash: null,
  displayName: "Search",
  getInitialState: function() {
    return {
      search: 'wut'
    };
  },
  onKeyUp: function(e) {
    return this.setState({
      search: e.target.value
    });
  },
  wrap: function(elem, dir, path) {
    var c, ga, gn, href, ref1;
    if (path.slice(-1) === "/") {
      path = path.slice(0, -1);
    }
    href = this.props.name + "/" + dir + path;
    if (elem != null ? (ref1 = elem.ga) != null ? ref1.id : void 0 : void 0) {
      gn = elem.gn, ga = elem.ga, c = elem.c;
      ga = _.clone(ga);
      href += "#" + ga.id;
      delete ga.id;
      elem = {
        gn: gn,
        ga: ga,
        c: c
      };
    }
    return {
      gn: 'div',
      c: [
        {
          gn: 'a',
          ga: {
            href: href
          },
          c: [elem]
        }
      ]
    };
  },
  render: function() {
    return div({}, input({
      onKeyUp: this.onKeyUp,
      ref: 'inp',
      defaultValue: 'wut'
    }), _(this.props.kids).map((function(_this) {
      return function(arg, dir) {
        var h, heds, path, results, sect;
        sect = arg.sect;
        results = [];
        for (path in sect) {
          heds = sect[path];
          results.push((function() {
            var i, len, results1;
            results1 = [];
            for (i = 0, len = heds.length; i < len; i++) {
              h = heds[i];
              results1.push(this.wrap(h, dir, path));
            }
            return results1;
          }).call(_this));
        }
        return results;
      };
    })(this)).flatten().flatten().map(this.highlight).filter().take(50).map(reactify).value());
  },
  highlight: function(e) {
    var got, res;
    if (!this.state.search) {
      return e;
    }
    got = false;
    res = reactify.walk(e, function() {
      return null;
    }, (function(_this) {
      return function(s) {
        var lit, m;
        m = s.split(_this.state.search);
        if (m[1] == null) {
          return [s];
        }
        lit = {
          gn: 'span',
          c: [_this.state.search],
          ga: {
            style: {
              background: '#ff6'
            }
          }
        };
        got = true;
        return [m[0]].concat(slice.call(_.flatten((function() {
            var i, len, ref1, results;
            ref1 = m.slice(1);
            results = [];
            for (i = 0, len = ref1.length; i < len; i++) {
              s = ref1[i];
              results.push([lit, s]);
            }
            return results;
          })())));
      };
    })(this), function(arg) {
      var c, ga, gn;
      gn = arg.gn, ga = arg.ga, c = arg.c;
      return {
        gn: gn,
        ga: ga,
        c: _.flatten(c)
      };
    });
    if (got) {
      return res;
    }
  }
}));


},{"./Async.coffee":3,"./Reactify.coffee":13}],16:[function(require,module,exports){
var a, clas, li, reactify, recl, ref, ul, util;

util = require('../utils/util.coffee');

clas = require('classnames');

reactify = require('./Reactify.coffee');

recl = React.createClass;

ref = React.DOM, ul = ref.ul, li = ref.li, a = ref.a;

module.exports = React.createFactory(recl({
  displayName: "Siblings",
  toText: function(elem) {
    return reactify.walk(elem, function() {
      return '';
    }, function(s) {
      return s;
    }, function(arg) {
      var c;
      c = arg.c;
      return (c != null ? c : []).join('');
    });
  },
  render: function() {
    var keys;
    keys = util.getKeys(this.props.kids);
    return ul({
      className: "nav"
    }, keys.map((function(_this) {
      return function(key) {
        var className, data, head, href;
        href = util.basepath(_this.props.path + "/" + key);
        data = _this.props.kids[key];
        if (data.meta) {
          head = data.meta.title;
        }
        if (head == null) {
          head = _this.toText(data.head);
        }
        head || (head = key);
        className = clas({
          "nav-item": true,
          selected: key === _this.props.curr
        });
        return li({
          className: className,
          key: key
        }, a({
          className: "nav-link",
          href: href,
          onClick: _this.props.toggleNav
        }, head));
      };
    })(this)));
  }
}));


},{"../utils/util.coffee":23,"./Reactify.coffee":13,"classnames":24}],17:[function(require,module,exports){
var div, query, reactify, recl,
  slice = [].slice;

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

recl = React.createClass;

div = React.DOM.div;

module.exports = query({
  body: 'r'
}, recl({
  hash: null,
  displayName: "TableOfContents",
  _click: function(id) {
    return function() {
      if (id) {
        return document.location.hash = id;
      }
    };
  },
  componentDidMount: function() {
    this.int = setInterval(this.checkHash, 100);
    this.st = $(window).scrollTop();
    return this.$headers = $('#toc').children('h1,h2,h3,h4').filter('[id]');
  },
  checkScroll: function() {
    var $h, hash, hst, i, len, ref, results, st, v;
    st = $(window).scrollTop();
    if (Math.abs(this.st - st) > 10) {
      hash = null;
      this.st = st;
      ref = this.$headers;
      results = [];
      for (i = 0, len = ref.length; i < len; i++) {
        v = ref[i];
        if (v.tagName === void 0) {
          continue;
        }
        $h = $(v);
        hst = $h.offset().top - $h.outerHeight(true) + 10;
        if (hst < st) {
          hash = $h.attr('id');
        }
        if (hst > st && hash !== this.hash && hash !== null) {
          this.hash = "#" + hash;
          document.location.hash = hash;
          break;
        } else {
          results.push(void 0);
        }
      }
      return results;
    }
  },
  checkHash: function() {
    var $h, hash, i, len, offset, ref, ref1, results, v;
    if (((ref = document.location.hash) != null ? ref.length : void 0) > 0 && document.location.hash !== this.hash) {
      hash = document.location.hash.slice(1);
      ref1 = this.$headers;
      results = [];
      for (i = 0, len = ref1.length; i < len; i++) {
        v = ref1[i];
        $h = $(v);
        if (hash === $h.attr('id')) {
          this.hash = document.location.hash;
          offset = $h.offset().top - $h.outerHeight(true);
          setTimeout(function() {
            return $(window).scrollTop(offset, 10);
          });
          break;
        } else {
          results.push(void 0);
        }
      }
      return results;
    }
  },
  componentWillUnmount: function() {
    return clearInterval(this.int);
  },
  collectHeader: function(arg) {
    var c, ga, gn;
    gn = arg.gn, ga = arg.ga, c = arg.c;
    if (gn && gn[0] === 'h' && parseInt(gn[1]) !== NaN) {
      ga = _.clone(ga);
      ga.onClick = this._click(ga.id);
      delete ga.id;
      return {
        gn: gn,
        ga: ga,
        c: c
      };
    }
  },
  parseHeaders: function() {
    var i, len, ref, ref1, v;
    if (this.props.body.c) {
      ref = this.props.body.c;
      for (i = 0, len = ref.length; i < len; i++) {
        v = ref[i];
        if (v.gn === 'div' && ((ref1 = v.ga) != null ? ref1.id : void 0) === "toc") {
          return {
            gn: "div",
            ga: {
              className: "toc"
            },
            c: [{
                gn: "h1",
                ga: {
                  className: "t"
                },
                c: ["Table of contents"]
              }].concat(slice.call(_.filter(v.c.map(this.collectHeader))))
          };
        }
      }
    }
  },
  render: function() {
    return reactify(this.parseHeaders());
  }
}));


},{"./Async.coffee":3,"./Reactify.coffee":13}],18:[function(require,module,exports){
module.exports = _.extend(new Flux.Dispatcher(), {
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


},{}],19:[function(require,module,exports){
var rend;

rend = ReactDOM.render;

$(function() {
  var body, frag, head, util;
  util = require('./utils/util.coffee');
  require('./utils/scroll.coffee');
  window.tree.actions = require('./actions/TreeActions.coffee');
  window.tree.actions.addVirtual(require('./components/Components.coffee'));
  frag = util.fragpath(window.location.pathname.replace(/\.[^\/]*$/, ''));
  window.tree.actions.setCurr(frag);
  window.tree.actions.loadPath(frag, window.tree.data);
  window.urb.ondataupdate = function(dep) {
    var dat;
    for (dat in window.urb.datadeps) {
      window.urb.dewasp(dat);
    }
    return window.tree.actions.clearData();
  };
  head = React.createFactory(require('./components/AnchorComponent.coffee'));
  body = React.createFactory(require('./components/BodyComponent.coffee'));
  rend(head({}, ""), $('#head')[0]);
  return rend(body({}, ""), $('#body')[0]);
});


},{"./actions/TreeActions.coffee":1,"./components/AnchorComponent.coffee":2,"./components/BodyComponent.coffee":4,"./components/Components.coffee":6,"./utils/scroll.coffee":22,"./utils/util.coffee":23}],20:[function(require,module,exports){
var dedup, util;

util = require('../utils/util.coffee');

dedup = {};

module.exports = {
  refresh: function() {
    return dedup = {};
  },
  get: function(path, query, cb) {
    var url;
    if (query == null) {
      query = "no-query";
    }
    url = (util.basepath(path)) + ".tree-json?q=" + (this.encode(query));
    if (dedup[url]) {
      return;
    }
    dedup[url] = true;
    return $.get(url, {}, function(data, status, xhr) {
      var dep;
      dep = urb.getXHRWasp(xhr);
      urb.sources[dep] = url;
      urb.waspData(dep);
      if (cb) {
        return cb(null, data);
      }
    });
  },
  put: function(mark, pax, txt) {
    return urb.send({
      pax: pax,
      txt: txt
    }, {
      mark: mark,
      appl: 'hood'
    });
  },
  encode: function(obj) {
    var _encode, delim;
    delim = function(n) {
      return Array(n + 1).join('_') || '.';
    };
    _encode = function(obj) {
      var _dep, dep, k, res, sub, v;
      if (typeof obj !== 'object') {
        return [0, obj];
      }
      dep = 0;
      sub = (function() {
        var ref, results;
        results = [];
        for (k in obj) {
          v = obj[k];
          ref = _encode(v), _dep = ref[0], res = ref[1];
          if (_dep > dep) {
            dep = _dep;
          }
          if (res != null) {
            results.push(k + (delim(_dep)) + res);
          } else {
            results.push(void 0);
          }
        }
        return results;
      })();
      dep++;
      return [dep, sub.join(delim(dep))];
    };
    return (_encode(obj))[1];
  }
};


},{"../utils/util.coffee":23}],21:[function(require,module,exports){
var EventEmitter, MessageDispatcher, QUERIES, TreeStore, _curr, _data, _nav, _tree, _virt, clog;

EventEmitter = require('events').EventEmitter.EventEmitter;

MessageDispatcher = require('../dispatcher/Dispatcher.coffee');

clog = console.log.bind(console);

_virt = {};

_tree = {};

_data = {};

_curr = "";

_nav = {};

QUERIES = {
  body: 'r',
  head: 'r',
  snip: 'r',
  sect: 'j',
  meta: 'j',
  comt: 'j'
};

TreeStore = _.extend((new EventEmitter).setMaxListeners(50), {
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
  fulfill: function(path, query) {
    if (path === "/") {
      path = "";
    }
    return this.fulfillAt(this.getTree(path.split('/')), path, query);
  },
  fulfillAt: function(tree, path, query) {
    var data, have, k, sub, t;
    data = this.fulfillLocal(path, query);
    have = _data[path];
    if (have != null) {
      for (k in query) {
        t = query[k];
        if (!QUERIES[k]) {
          continue;
        }
        if (t !== QUERIES[k]) {
          throw TypeError("Wrong query type: " + k + ", '" + t + "'");
        }
        data[k] = have[k];
      }
      if (query.kids) {
        if (have.kids === false) {
          data.kids = {};
        } else {
          for (k in tree) {
            sub = tree[k];
            if (data.kids == null) {
              data.kids = {};
            }
            data.kids[k] = this.fulfillAt(sub, path + "/" + k, query.kids);
          }
        }
      }
    }
    if (!_.isEmpty(data)) {
      return data;
    }
  },
  fulfillLocal: function(path, query) {
    var data;
    data = {};
    if (query.path) {
      data.path = path;
    }
    if (query.name) {
      data.name = path.split("/").pop();
    }
    if (query.sein) {
      data.sein = this.getPare(path);
    }
    if (query.next) {
      data.next = this.getNext(path);
    }
    if (query.prev) {
      data.prev = this.getPrev(path);
    }
    return data;
  },
  setCurr: function(arg) {
    var path;
    path = arg.path;
    return _curr = path;
  },
  getCurr: function() {
    return _curr;
  },
  addVirtual: function(arg) {
    var components;
    components = arg.components;
    return _.extend(_virt, components);
  },
  getVirtualComponents: function() {
    return _virt;
  },
  clearData: function() {
    _data = {};
    return _tree = {};
  },
  loadPath: function(arg) {
    var data, path;
    path = arg.path, data = arg.data;
    return this.loadValues(this.getTree(path.split('/'), true), path, data);
  },
  loadValues: function(tree, path, data) {
    var k, old, ref, ref1, v;
    old = (ref = _data[path]) != null ? ref : {};
    for (k in data) {
      if (QUERIES[k]) {
        old[k] = data[k];
      }
    }
    ref1 = data.kids;
    for (k in ref1) {
      v = ref1[k];
      if (tree[k] == null) {
        tree[k] = {};
      }
      this.loadValues(tree[k], path + "/" + k, v);
    }
    if (data.kids && _.isEmpty(data.kids)) {
      old.kids = false;
    }
    return _data[path] = old;
  },
  getSiblings: function(path) {
    var curr;
    if (path == null) {
      path = _curr;
    }
    curr = path.split("/");
    curr.pop();
    if (curr.length !== 0) {
      return this.getTree(curr);
    } else {
      return {};
    }
  },
  getTree: function(_path, make) {
    var i, len, sub, tree;
    if (make == null) {
      make = false;
    }
    tree = _tree;
    for (i = 0, len = _path.length; i < len; i++) {
      sub = _path[i];
      if (tree[sub] == null) {
        if (!make) {
          return null;
        }
        tree[sub] = {};
      }
      tree = tree[sub];
    }
    return tree;
  },
  getPrev: function(path) {
    var ind, key, par, sibs, win;
    if (path == null) {
      path = _curr;
    }
    sibs = _.keys(this.getSiblings(path)).sort();
    if (sibs.length < 2) {
      return null;
    } else {
      par = path.split("/");
      key = par.pop();
      ind = sibs.indexOf(key);
      win = ind - 1 >= 0 ? sibs[ind - 1] : sibs[sibs.length - 1];
      par.push(win);
      return par.join("/");
    }
  },
  getNext: function(path) {
    var ind, key, par, sibs, win;
    if (path == null) {
      path = _curr;
    }
    sibs = _.keys(this.getSiblings(path)).sort();
    if (sibs.length < 2) {
      return null;
    } else {
      par = path.split("/");
      key = par.pop();
      ind = sibs.indexOf(key);
      win = ind + 1 < sibs.length ? sibs[ind + 1] : sibs[0];
      par.push(win);
      return par.join("/");
    }
  },
  getPare: function(path) {
    var _path;
    if (path == null) {
      path = _curr;
    }
    _path = this.pathToArr(path);
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
  setNav: function(arg) {
    var dpad, nav, sibs, subnav, title;
    title = arg.title, dpad = arg.dpad, sibs = arg.sibs, subnav = arg.subnav;
    nav = {
      title: title,
      dpad: dpad,
      sibs: sibs,
      subnav: subnav,
      open: (_nav.open ? _nav.open : false)
    };
    return _nav = nav;
  },
  getNav: function() {
    return _nav;
  },
  toggleNav: function() {
    return _nav.open = !_nav.open;
  },
  clearNav: function() {
    return _nav = {
      title: null,
      dpad: null,
      sibs: null,
      subnav: null,
      open: false
    };
  }
});

TreeStore.dispatchToken = MessageDispatcher.register(function(p) {
  var a;
  a = p.action;
  if (TreeStore[a.type]) {
    TreeStore[a.type](a);
    return TreeStore.emitChange();
  }
});

module.exports = TreeStore;


},{"../dispatcher/Dispatcher.coffee":18,"events":25}],22:[function(require,module,exports){
var scroll;

scroll = {
  w: null,
  $d: null,
  $n: null,
  nh: null,
  cs: null,
  ls: null,
  track: function() {
    this.w = $(window).width();
    this.$n = $('.nav.container');
    return this.nh = $('.nav.container .ctrl').outerHeight(true);
  },
  clearNav: function() {
    return this.$n.removeClass('m-up m-down m-fixed');
  },
  resize: function() {
    if (this.w > 1170) {
      return this.clearNav();
    }
  },
  scroll: function() {
    var ct, dy, top;
    this.cs = $(window).scrollTop();
    if (this.w > 1170) {
      this.clearNav();
    }
    if (this.w < 1170) {
      dy = this.ls - this.cs;
      this.$d.removeClass('focus');
      if (this.cs <= 0) {
        this.$n.removeClass('m-up');
        this.$n.addClass('m-down m-fixed');
        return;
      }
      if (dy > 0) {
        if (!this.$n.hasClass('m-down')) {
          this.$n.removeClass('m-up').addClass('m-down');
          ct = this.$n.offset().top;
          top = this.cs - this.nh;
          if (this.cs > ct && this.cs < ct + this.nh) {
            top = ct;
          }
          this.$n.offset({
            top: this.$n.top
          });
        }
        if (this.$n.hasClass('m-down') && !this.$n.hasClass('m-fixed') && this.$n.offset().top >= this.cs) {
          this.$n.addClass('m-fixed');
          this.$n.attr({
            style: ''
          });
        }
      }
      if (dy < 0) {
        if (!this.$n.hasClass('m-up')) {
          this.$n.removeClass('m-down m-fixed').addClass('m-up');
          top = this.cs < 0 ? 0 : this.cs;
          ct = this.$n.offset().top;
          if (top > ct && top < ct + this.nh) {
            top = ct;
          }
          this.$n.offset({
            top: top
          });
        }
        if (this.$n.hasClass('m-up') && this.$d.hasClass('open')) {
          if (this.cs > this.$n.offset().top + this.$n.height()) {
            this.$d.removeClass('open');
          }
        }
      }
    }
    return this.ls = this.cs;
  },
  init: function() {
    setInterval(this.track.bind(this), 200);
    this.ls = $(window).scrollTop();
    this.cs = $(window).scrollTop();
    this.$d = $('.nav.container .ctrl');
    $(window).on('resize', this.resize.bind(this));
    return $(window).on('scroll', this.scroll.bind(this));
  }
};

scroll.init();

module.exports = scroll;


},{}],23:[function(require,module,exports){
var _basepath;

_basepath = window.urb.util.basepath("/");

_basepath += (window.location.pathname.replace(window.tree._basepath, "")).split("/")[0];

module.exports = {
  basepath: function(path) {
    var _path, prefix;
    prefix = _basepath;
    if (prefix === "/") {
      prefix = "";
    }
    if (path[0] !== "/") {
      path = "/" + path;
    }
    _path = prefix + path;
    if (_path.slice(-1) === "/" && _path.length > 1) {
      _path = _path.slice(0, -1);
    }
    return _path;
  },
  fragpath: function(path) {
    return path.replace(/\/$/, '').replace(_basepath, "");
  },
  getKeys: function(kids) {
    var k, keys, ref, ref1, ref2, sorted, v;
    sorted = true;
    keys = [];
    for (k in kids) {
      v = kids[k];
      if ((ref = v.meta) != null ? ref.hide : void 0) {
        continue;
      }
      if (((ref1 = v.meta) != null ? ref1.sort : void 0) == null) {
        sorted = false;
      }
      keys[Number((ref2 = v.meta) != null ? ref2.sort : void 0)] = k;
    }
    if (sorted !== true) {
      return keys = _.keys(kids).sort();
    } else {
      return keys = _.values(keys);
    }
  }
};


},{}],24:[function(require,module,exports){
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

},{}],25:[function(require,module,exports){
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

},{}]},{},[19]);
