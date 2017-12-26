(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var TreeDispatcher, TreePersistence, _initialLoad, _initialLoadDedup;

TreeDispatcher = require('../dispatcher/Dispatcher.coffee');

TreePersistence = require('../persistence/TreePersistence.coffee');

_initialLoad = true;

_initialLoadDedup = {};

module.exports = {
  loadPath: function(path, data) {
    return TreeDispatcher.handleServerAction({
      path: path,
      data: data,
      type: "loadPath"
    });
  },
  loadSein: function(path, data) {
    return TreeDispatcher.handleServerAction({
      path: path,
      data: data,
      type: "loadSein"
    });
  },
  clearData: function() {
    _initialLoad = false;
    _initialLoadDedup = {};
    TreePersistence.refresh();
    return TreeDispatcher.handleServerAction({
      type: "clearData"
    });
  },
  sendQuery: function(path, query) {
    var key;
    if (query == null) {
      return;
    }
    if (_initialLoad) {
      key = path + (JSON.stringify(query));
      if (!_initialLoadDedup[key]) {
        _initialLoadDedup[key] = true;
        console.warn("Requesting data during initial page load", JSON.stringify(path), query);
      }
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
  registerScriptElement: function(elem) {
    return TreePersistence.waspElem(elem);
  },
  addVirtual: function(components) {
    return TreeDispatcher.handleViewAction({
      type: "addVirtual",
      components: components
    });
  },
  addComment: function(pax, sup, txt) {
    return TreePersistence.put({
      pax: pax,
      sup: sup,
      txt: txt
    }, "fora-comment", "fora", (function(_this) {
      return function(err, res) {
        if (err == null) {
          return _this.clearData();
        }
      };
    })(this));
  },
  addPost: function(pax, sup, hed, txt) {
    return TreePersistence.put({
      pax: pax,
      sup: sup,
      hed: hed,
      txt: txt
    }, "fora-post", "fora", (function(_this) {
      return function(err, res) {
        if (err == null) {
          _this.clearData();
          history.pushState({}, "", "..");
          return _this.setCurr(pax);
        }
      };
    })(this));
  },
  setPlanInfo: function(arg) {
    var loc, who;
    who = arg.who, loc = arg.loc;
    return TreePersistence.put({
      who: who,
      loc: loc
    }, "write-plan-info", "hood");
  },
  setCurr: function(path, init) {
    if (init == null) {
      init = false;
    }
    _initialLoad &= init;
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
  closeNav: function() {
    return TreeDispatcher.handleViewAction({
      type: "closeNav"
    });
  },
  clearNav: function() {
    return TreeDispatcher.handleViewAction({
      type: "clearNav"
    });
  }
};


},{"../dispatcher/Dispatcher.coffee":25,"../persistence/TreePersistence.coffee":27}],2:[function(require,module,exports){
var TreeActions, TreeStore, _load, code, div, fragsrc, recl, ref, span, util;

util = require('../utils/util.coffee');

_load = require('./LoadComponent.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, span = ref.span, code = ref.code;

fragsrc = function(src, basePath) {
  var base, pathname;
  if (src != null) {
    basePath = util.basepath(basePath);
    if (basePath.slice(-1) !== "/") {
      basePath += "/";
    }
    base = new URL(basePath, document.location);
    pathname = new URL(src, base).pathname;
    return util.fragpath(pathname);
  }
};

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
      var base, path, ref1, ref2;
      path = this.props.dataPath;
      base = (ref1 = this.props.basePath) != null ? ref1 : TreeStore.getCurr();
      if (path == null) {
        path = (ref2 = fragsrc(this.props.src, base)) != null ? ref2 : base;
      }
      if (path.slice(-1) === "/") {
        return path.slice(0, -1);
      } else {
        return path;
      }
    },
    stateFromStore: function() {
      var fresh, got, path;
      path = this.getPath();
      fresh = TreeStore.fulfill(path, queries);
      if (!((this.state != null) && path === this.state.path)) {
        got = fresh;
      } else {
        got = this.mergeWith(this.state.got, fresh);
      }
      return {
        path: path,
        fresh: fresh,
        got: got,
        queries: queries
      };
    },
    mergeWith: function(have, fresh, _queries) {
      var got, k, kid, ref1, ref2, ref3;
      if (have == null) {
        have = {};
      }
      if (fresh == null) {
        fresh = {};
      }
      if (_queries == null) {
        _queries = queries;
      }
      got = {};
      for (k in _queries) {
        if (k !== 'kids') {
          got[k] = (ref1 = fresh[k]) != null ? ref1 : have[k];
        }
      }
      if (_queries.kids != null) {
        if (fresh.kids == null) {
          got.kids = have.kids;
        } else {
          got.kids = {};
          ref2 = fresh.kids;
          for (k in ref2) {
            kid = ref2[k];
            got.kids[k] = this.mergeWith((ref3 = have.kids) != null ? ref3[k] : void 0, kid, _queries.kids);
          }
        }
      }
      return got;
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


},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":28,"../utils/util.coffee":30,"./LoadComponent.coffee":12}],3:[function(require,module,exports){
var Comments, TreeActions, TreeStore, a, clas, div, extras, h1, h3, img, input, load, name, p, query, reactify, recl, ref, rele, util;

clas = require('classnames');

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeActions = require('../actions/TreeActions.coffee');

TreeStore = require('../stores/TreeStore.coffee');

Comments = require('./CommentsComponent.coffee');

util = require('../utils/util.coffee');

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, h1 = ref.h1, h3 = ref.h3, p = ref.p, img = ref.img, a = ref.a, input = ref.input;

extras = {
  spam: name("Spam", function() {
    if (document.location.hostname !== 'urbit.org') {
      return div({});
    }
    return div({
      className: 'spam'
    }, a({
      href: "http://urbit.org#sign-up"
    }, "Sign up"), " for our newsletter.");
  }),
  logo: name("Logo", function(arg) {
    var color, src;
    color = arg.color;
    if (color === "white" || color === "black") {
      src = "//media.urbit.org/logo/logo-" + color + "-100x100.png";
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
  }),
  date: name("Date", function(arg) {
    var date;
    date = arg.date;
    return div({
      className: 'date'
    }, date);
  }),
  title: name("Title", function(arg) {
    var title;
    title = arg.title;
    return h1({
      className: 'title'
    }, title);
  }),
  image: name("Image", function(arg) {
    var image;
    image = arg.image;
    return img({
      src: image
    });
  }),
  preview: name("Preview", function(arg) {
    var preview;
    preview = arg.preview;
    return p({
      className: 'preview'
    }, preview);
  }),
  author: name("Author", function(arg) {
    var author;
    author = arg.author;
    return h3({
      className: 'author'
    }, author);
  }),
  next: query({
    path: 't',
    kids: {
      name: 't',
      head: 'r',
      meta: 'j',
      bump: 't'
    }
  }, name("Next", function(arg) {
    var curr, index, keys, kids, meta, next, path, ref1, ref2;
    curr = arg.curr, meta = arg.meta, path = arg.path, kids = arg.kids;
    if ((ref1 = kids[curr]) != null ? (ref2 = ref1.meta) != null ? ref2.next : void 0 : void 0) {
      keys = util.getKeys(kids, meta.navsort);
      if (keys.length > 1) {
        index = keys.indexOf(curr);
        next = index + 1;
        if (next === keys.length) {
          next = 0;
        }
        next = keys[next];
        next = kids[next];
        if (next) {
          return div({
            className: "link-next"
          }, a({
            href: path + "/" + next.name
          }, "Next: " + next.meta.title));
        }
      }
    }
    return div({}, "");
  })),
  comments: Comments,
  footer: name("Footer", function(arg) {
    var container, containerClas, footerClas;
    container = arg.container;
    containerClas = clas({
      footer: true,
      container: container === 'false'
    });
    footerClas = clas({
      'col-md-12': container === 'false'
    });
    return div({
      className: containerClas,
      key: 'footer-container'
    }, [
      div({
        className: footerClas,
        key: 'footer-inner'
      }, [
        "This page was made by Urbit. Feedback: ", a({
          href: "mailto:urbit@urbit.org"
        }, "urbit@urbit.org"), " ", a({
          href: "https://twitter.com/urbit"
        }, "@urbit")
      ])
    ]);
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
  stateFromStore: function() {
    return {
      curr: TreeStore.getCurr()
    };
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
  render: function() {
    var bodyClas, extra, innerClas, parts, ref1;
    extra = (function(_this) {
      return function(name, props) {
        if (props == null) {
          props = {};
        }
        if (_this.props.meta[name] != null) {
          if ((_.keys(props)).length === 0) {
            props[name] = _this.props.meta[name];
          }
          props.key = name;
          return React.createElement(extras[name], props);
        }
      };
    })(this);
    innerClas = {
      body: true
    };
    if (this.props.meta.anchor !== 'none' && this.props.meta.navmode !== 'navbar') {
      innerClas['col-md-9'] = true;
      innerClas['col-md-offset-3'] = true;
    }
    if (this.props.meta.navmode === 'navbar' && this.props.meta.container !== 'false') {
      innerClas['col-md-9'] = true;
      innerClas['col-md-offset-1'] = true;
    }
    innerClas = clas(innerClas);
    bodyClas = clas((ref1 = this.props.meta.layout) != null ? ref1.split(',') : void 0);
    if (this.props.meta.type && bodyClas.indexOf(this.props.meta.type) === -1) {
      bodyClas += " " + this.props.meta.type;
    }
    parts = [
      extra('spam'), extra('logo', {
        color: this.props.meta.logo
      }), reactify(this.props.body, 'body'), extra('next', {
        dataPath: this.props.sein,
        curr: this.props.name,
        meta: this.props.meta
      }), extra('comments'), extra('footer', {
        container: this.props.meta.container
      })
    ];
    if (this.props.meta.type === "post") {
      parts.splice(1, 0, extra('date'), extra('title'), extra('image'), extra('preview'), extra('author'));
    }
    return div({
      dataPath: this.state.curr,
      key: this.state.curr
    }, [
      div({
        className: innerClas,
        'data-path': this.props.path,
        key: 'body-inner'
      }, [
        div({
          key: "body" + this.props.path,
          id: 'body',
          className: bodyClas
        }, parts)
      ])
    ]);
  }
}), recl({
  render: function() {
    return div({
      id: 'body',
      className: "col-md-offset-3 col-md-9"
    }, rele(load));
  }
}));


},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":28,"../utils/util.coffee":30,"./Async.coffee":2,"./CommentsComponent.coffee":5,"./LoadComponent.coffee":12,"./Reactify.coffee":18,"classnames":31}],4:[function(require,module,exports){
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


},{}],5:[function(require,module,exports){
var Comment, DEFER_USER, Ship, TreeActions, a, clas, code, div, form, h2, img, input, load, p, query, reactify, recl, ref, rele, textarea, util,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

clas = require('classnames');

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeActions = require('../actions/TreeActions.coffee');

util = require('../utils/util.coffee');

Ship = require('./ShipComponent.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, p = ref.p, h2 = ref.h2, img = ref.img, a = ref.a, form = ref.form, textarea = ref.textarea, input = ref.input, code = ref.code;

DEFER_USER = true;

Comment = function(arg) {
  var body, loading, ref1, time, user;
  time = arg.time, user = arg.user, body = arg.body, loading = (ref1 = arg.loading) != null ? ref1 : false;
  return div({
    className: clas("comment", {
      loading: loading
    })
  }, "" + (window.urb.util.toDate(new Date(time))), h2({}, rele(Ship, {
    ship: user
  })), reactify(body, "comt", {
    components: {}
  }));
};

module.exports = query({
  comt: 'j',
  path: 't',
  spur: 't',
  meta: 'j'
}, recl({
  displayName: "Comments",
  getInitialState: function() {
    var ref1;
    return {
      loading: null,
      value: "",
      user: (ref1 = urb.user) != null ? ref1 : ""
    };
  },
  componentDidMount: function() {
    if (!DEFER_USER) {
      return urb.init((function(_this) {
        return function() {
          return _this.setState({
            user: urb.user
          });
        };
      })(this));
    }
  },
  componentDidUpdate: function(_props) {
    var ref1;
    if (urb.user && !this.state.user) {
      this.setState({
        user: (ref1 = urb.user) != null ? ref1 : ""
      });
    }
    if (this.props.comt.length > _props.comt.length) {
      return this.setState({
        loading: null
      });
    }
  },
  onSubmit: function(e) {
    var value;
    value = this.refs["in"].comment.value;
    TreeActions.addComment(this.props.path, this.props.spur, value);
    this.setState({
      value: "",
      loading: {
        'loading': 'loading',
        body: {
          gn: 'p',
          c: [value]
        },
        time: Date.now()
      }
    });
    return e.preventDefault();
  },
  onChange: function(e) {
    return this.setState({
      value: e.target.value
    });
  },
  render: function() {
    var _attr, addComment, comments, inputAttr, ref1, ref2, textareaAttr;
    _attr = {};
    if (this.state.loading === true) {
      _attr.disabled = "true";
    }
    textareaAttr = _.create(_attr, {
      type: "text",
      name: "comment",
      value: this.state.value,
      onChange: this.onChange
    });
    inputAttr = _.create(_attr, {
      type: "submit",
      value: "Add comment",
      className: "btn btn-primary"
    });
    addComment = div({
      key: 'add-comment',
      className: "add-comment"
    }, form({
      ref: "in",
      onSubmit: this.onSubmit
    }, rele(Ship, {
      ship: this.state.user
    }), textarea(textareaAttr), input(inputAttr)));
    comments = this.props.comt.map(function(props, key) {
      return rele(Comment, _.extend({
        key: key
      }, props));
    });
    comments.unshift((this.state.loading != null ? rele(Comment, _.extend({
      key: 'loading'
    }, this.state.loading, {
      user: this.state.user
    })) : void 0));
    if (indexOf.call((ref1 = (ref2 = this.props.meta.comments) != null ? ref2.split(" ") : void 0) != null ? ref1 : [], "reverse") >= 0) {
      comments = comments.reverse();
      return div({}, [
        div({
          key: 'comments',
          className: "comments"
        }, comments), addComment
      ]);
    } else {
      return div({}, [
        addComment, div({
          key: 'comments',
          className: "comments"
        }, comments)
      ]);
    }
  }
}));


},{"../actions/TreeActions.coffee":1,"../utils/util.coffee":30,"./Async.coffee":2,"./LoadComponent.coffee":12,"./Reactify.coffee":18,"./ShipComponent.coffee":21,"classnames":31}],6:[function(require,module,exports){
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
  plan: require('./PlanComponent.coffee'),
  panel: require('./PanelComponent.coffee'),
  post: require('./PostComponent.coffee'),
  imagepanel: require('./ImagepanelComponent.coffee'),
  load: require('./LoadComponent.coffee'),
  ship: require('./ShipComponent.coffee'),
  lost: recl({
    render: function() {
      return div({}, "<lost(", this.props.children, ")>");
    }
  })
};


},{"./CodeMirror.coffee":4,"./EmailComponent.coffee":8,"./ImagepanelComponent.coffee":9,"./KidsComponent.coffee":10,"./ListComponent.coffee":11,"./LoadComponent.coffee":12,"./ModuleComponent.coffee":13,"./PanelComponent.coffee":15,"./PlanComponent.coffee":16,"./PostComponent.coffee":17,"./ScriptComponent.coffee":19,"./SearchComponent.coffee":20,"./ShipComponent.coffee":21,"./TocComponent.coffee":23}],7:[function(require,module,exports){
var Arrow, Dpad, a, div, recl, ref, util;

util = require('../utils/util.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, a = ref.a;

Arrow = function(name, path) {
  var href;
  href = util.basepath(path);
  return a({
    href: href,
    key: "" + name,
    className: "" + name
  }, "");
};

module.exports = Dpad = function(arg) {
  var arrowSibs, arrowUp, curr, index, keys, kids, meta, next, prev, sein;
  sein = arg.sein, curr = arg.curr, kids = arg.kids, meta = arg.meta;
  arrowUp = sein ? meta.navuptwo ? Arrow("up", sein.replace(/\/[^\/]*$/, "")) : Arrow("up", sein) : void 0;
  arrowSibs = (keys = util.getKeys(kids, meta.navsort), keys.length > 1 ? (index = keys.indexOf(curr), prev = index - 1, next = index + 1, prev < 0 ? prev = keys.length - 1 : void 0, next === keys.length ? next = 0 : void 0, prev = keys[prev], next = keys[next]) : void 0, sein ? (sein === "/" ? sein = "" : void 0, div({}, prev ? Arrow("prev", sein + "/" + prev) : void 0, next ? Arrow("next", sein + "/" + next) : void 0)) : void 0);
  return div({
    className: 'dpad',
    key: 'dpad'
  }, arrowUp, arrowSibs);
};


},{"../utils/util.coffee":30}],8:[function(require,module,exports){
var button, div, input, p, recl, ref;

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
  onChange: function(e) {
    var email, valid;
    email = e.target.value;
    this.setState({
      email: e.target.value
    });
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
    var cont, ref1, submit;
    if (this.state.submit === false) {
      submit = (ref1 = this.props.submit) != null ? ref1 : "Sign up";
      cont = [
        input({
          key: "field",
          className: "email",
          placeholder: "your@email.com",
          onChange: this.onChange,
          value: this.state.email
        }), button({
          key: "submit",
          className: "submit btn",
          onClick: this.onClick
        }, submit)
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


},{}],9:[function(require,module,exports){
var div, name, recl;

recl = React.createClass;

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

div = React.DOM.div;

module.exports = name("ImagePanel", function(arg) {
  var src;
  src = arg.src;
  return div({
    className: "image-container",
    style: {
      backgroundImage: "url('" + src + "')"
    }
  });
});


},{}],10:[function(require,module,exports){
var a, clas, div, hr, li, query, reactify, recl, ref, ul, util;

clas = require('classnames');

util = require('../utils/util.coffee');

reactify = require('./Reactify.coffee');

query = require('./Async.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, a = ref.a, ul = ref.ul, li = ref.li, hr = ref.hr;

module.exports = query({
  kids: {
    name: 't',
    bump: 't',
    body: 'r',
    meta: 'j',
    path: 't'
  }
}, recl({
  displayName: "Kids",
  render: function() {
    var body, elem, kidClas, kids, kidsClas;
    kids = util.sortKids(this.props.kids, this.props.sortBy);
    kidsClas = clas({
      kids: true
    }, this.props.className);
    kidClas = clas({
      "col-md-4": this.props.grid === 'true'
    });
    return div({
      className: kidsClas,
      key: "kids"
    }, (function() {
      var i, len, results;
      results = [];
      for (i = 0, len = kids.length; i < len; i++) {
        elem = kids[i];
        body = reactify(elem.body, null, {
          basePath: elem.path
        });
        results.push([
          div({
            key: elem.name,
            id: elem.name,
            className: kidClas
          }, body), hr({})
        ]);
      }
      return results;
    })());
  }
}));


},{"../utils/util.coffee":30,"./Async.coffee":2,"./Reactify.coffee":18,"classnames":31}],11:[function(require,module,exports){
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
    meta: 'j',
    bump: 't',
    name: 't'
  }
}, recl({
  displayName: "List",
  render: function() {
    var k, kids;
    k = clas({
      list: true
    }, this.props.dataType, {
      "default": this.props['data-source'] === 'default'
    }, this.props.className);
    kids = this.renderList(util.sortKids(this.props.kids, this.props.sortBy));
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
  renderList: function(elems) {
    var _date, author, cont, date, elem, href, i, image, item, len, linked, meta, node, parts, path, preview, ref1, results, title;
    results = [];
    for (i = 0, len = elems.length; i < len; i++) {
      elem = elems[i];
      item = elem.name;
      meta = (ref1 = elem.meta) != null ? ref1 : {};
      path = this.props.path + "/" + item;
      if (meta.hide != null) {
        continue;
      }
      href = util.basepath(path);
      if (this.props.linkToFragments != null) {
        href = "#" + item;
      }
      if (this.props.childIsFragment != null) {
        href = (util.basepath(this.props.path)) + "#" + item;
      }
      if (meta.link) {
        href = meta.link;
      }
      parts = [];
      title = null;
      if (meta.title) {
        if (this.props.dataType === 'post') {
          title = {
            gn: 'a',
            ga: {
              href: href
            },
            c: [
              {
                gn: 'h1',
                ga: {
                  className: 'title'
                },
                c: [meta.title]
              }
            ]
          };
        } else {
          title = {
            gn: 'h1',
            ga: {
              className: 'title'
            },
            c: [meta.title]
          };
        }
      }
      if (!title && elem.head.c.length > 0) {
        title = elem.head;
      }
      if (!title) {
        title = {
          gn: 'h1',
          ga: {
            className: 'title'
          },
          c: [item]
        };
      }
      if (!this.props.titlesOnly) {
        _date = meta.date;
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
      parts.push(title);
      if (!this.props.titlesOnly) {
        if (this.props.dataType === 'post') {
          if (meta.image) {
            image = {
              gn: 'a',
              ga: {
                href: href
              },
              c: [
                {
                  gn: 'img',
                  ga: {
                    src: meta.image
                  }
                }
              ]
            };
            parts.push(image);
          }
        }
        if (this.props.dataPreview) {
          if (!meta.preview) {
            parts.push.apply(parts, elem.snip.c.slice(0, 2));
          } else {
            if (meta.preview) {
              preview = {
                gn: 'p',
                ga: {
                  className: 'preview'
                },
                c: [meta.preview]
              };
            } else {
              preview = elem.snip;
            }
            parts.push(preview);
          }
        }
        if (this.props.dataType === 'post') {
          if (meta.author) {
            author = {
              gn: 'h3',
              ga: {
                className: 'author'
              },
              c: [meta.author]
            };
            parts.push(author);
          }
          cont = {
            gn: 'a',
            ga: {
              className: 'continue',
              href: href
            },
            c: ['Read more']
          };
          parts.push(cont);
          linked = true;
        }
      }
      node = reactify({
        gn: 'div',
        c: parts
      });
      if (linked == null) {
        node = a({
          href: href,
          className: clas({
            preview: this.props.dataPreview != null
          })
        }, node);
      }
      results.push(li({
        key: item
      }, node));
    }
    return results;
  }
}));


},{"../utils/util.coffee":30,"./Async.coffee":2,"./Reactify.coffee":18,"classnames":31}],12:[function(require,module,exports){
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


},{}],13:[function(require,module,exports){
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
    return setTimeout((function() {
      return TreeActions.clearNav();
    }), 0);
  },
  render: function() {
    return div({
      className: "module"
    }, this.props.children);
  }
});


},{"../actions/TreeActions.coffee":1}],14:[function(require,module,exports){
var BodyComponent, Dpad, Nav, Sibs, TreeActions, TreeStore, a, button, clas, div, li, query, reactify, recl, ref, rend, ul, util;

clas = require('classnames');

BodyComponent = React.createFactory(require('./BodyComponent.coffee'));

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

Sibs = React.createFactory(require('./SibsComponent.coffee'));

Dpad = React.createFactory(require('./DpadComponent.coffee'));

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
    return this.props.goTo(this.props.meta.navhome ? this.props.meta.navhome : "/");
  },
  toggleFocus: function(state) {
    return $(ReactDOM.findDOMNode(this)).toggleClass('focus', state);
  },
  toggleNav: function() {
    return TreeActions.toggleNav();
  },
  closeNav: function() {
    return TreeActions.closeNav();
  },
  render: function() {
    var attr, dpad, i, iconClass, itemsClass, len, linksClas, navClas, ref1, ref2, ref3, sibs, sub, subprops, title, toggleClas, v;
    attr = {
      onMouseOver: this.onMouseOver,
      onMouseOut: this.onMouseOut,
      onClick: this.onClick,
      onTouchStart: this.onTouchStart,
      onTouchEnd: this.onTouchEnd,
      'data-path': this.props.dataPath
    };
    if (_.keys(window).indexOf("ontouchstart") !== -1) {
      delete attr.onMouseOver;
      delete attr.onMouseOut;
    }
    linksClas = clas({
      links: true,
      subnav: (this.props.meta.navsub != null)
    });
    navClas = {
      navbar: this.props.meta.navmode === 'navbar',
      ctrl: true,
      open: this.state.open === true
    };
    if (this.props.meta.layout) {
      ref1 = this.props.meta.layout.split(",");
      for (i = 0, len = ref1.length; i < len; i++) {
        v = ref1[i];
        navClas[v.trim()] = true;
      }
    }
    navClas = clas(navClas);
    iconClass = clas({
      icon: true,
      'col-md-1': this.props.meta.navmode === 'navbar'
    });
    attr = _.extend(attr, {
      className: navClas,
      key: "nav"
    });
    title = this.state.title ? this.state.title : "";
    dpad = this.state.dpad !== false && ((ref2 = this.props.meta) != null ? ref2.navdpad : void 0) !== "false" ? Dpad(this.props, "") : "";
    sibs = this.state.sibs !== false && ((ref3 = this.props.meta) != null ? ref3.navsibs : void 0) !== "false" ? Sibs(_.merge(_.clone(this.props), {
      closeNav: this.closeNav
    }), "") : "";
    itemsClass = clas({
      items: true,
      'col-md-11': this.props.meta.navmode === 'navbar'
    });
    if (this.props.meta.navsub) {
      subprops = _.cloneDeep(this.props);
      subprops.dataPath = subprops.meta.navsub;
      delete subprops.meta.navselect;
      subprops.className = 'subnav';
      sub = Sibs(_.merge(subprops, {
        toggleNav: this.toggleNav
      }), "");
    }
    toggleClas = clas({
      'navbar-toggler': true,
      show: this.state.subnav != null
    });
    return div(attr, div({
      className: linksClas,
      key: "links"
    }, div({
      className: iconClass
    }, div({
      className: 'home',
      onClick: this._home
    }, ""), div({
      className: 'app'
    }, title), dpad, button({
      className: toggleClas,
      type: 'button',
      onClick: this.toggleNav
    }, "☰")), div({
      className: itemsClass
    }, sibs, sub)));
  }
}), recl({
  displayName: "Links_loading",
  _home: function() {
    return this.props.goTo("/");
  },
  render: function() {
    return div({
      className: "ctrl loading",
      "data-path": this.props.dataPath,
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
  displayName: "Nav",
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
    this.setTitle();
    return this.checkRedirect();
  },
  componentDidMount: function() {
    var _this;
    this.setTitle();
    window.onpopstate = this.pullPath;
    TreeStore.addChangeListener(this._onChangeStore);
    _this = this;
    $('body').on('click', 'a', function(e) {
      var basepath, href, url;
      if (e.shiftKey || e.ctrlKey || e.metaKey) {
        return true;
      }
      href = $(this).attr('href');
      if ((href != null ? href[0] : void 0) === "#") {
        return true;
      }
      if (href && !/^https?:\/\//i.test(href)) {
        url = new URL(this.href);
        if (!/http/.test(url.protocol)) {
          return;
        }
        e.preventDefault();
        basepath = urb.util.basepath;
        if (basepath("", url.pathname) !== basepath("", document.location.pathname)) {
          document.location = this.href;
          return;
        }
        if (url.pathname.substr(-1) !== "/") {
          url.pathname += "/";
        }
        return _this.goTo(url.pathname + url.search + url.hash);
      }
    });
    return this.checkRedirect();
  },
  checkRedirect: function() {
    if (this.props.meta.redirect) {
      return setTimeout(((function(_this) {
        return function() {
          return _this.goTo(_this.props.meta.redirect);
        };
      })(this)), 0);
    }
  },
  setTitle: function() {
    var path, ref1, title;
    title = $('#body h1').first().text() || this.props.name;
    if ((ref1 = this.props.meta) != null ? ref1.title : void 0) {
      title = this.props.meta.title;
    }
    path = this.props.path;
    if (path === "") {
      path = "/";
    }
    return document.title = title + " - " + path;
  },
  pullPath: function() {
    var l, path;
    l = document.location;
    path = l.pathname + l.search + l.hash;
    return this.setPath(path, false);
  },
  setPath: function(path, hist) {
    var next;
    if (hist !== false) {
      history.pushState({}, "", path);
    }
    next = util.fragpath(path.split('#')[0]);
    if (next !== this.props.path) {
      return TreeActions.setCurr(next);
    }
  },
  reset: function() {
    return $("html,body").animate({
      scrollTop: 0
    });
  },
  goTo: function(path) {
    this.reset();
    return this.setPath(path);
  },
  render: function() {
    var kids, kidsPath, navClas;
    if (this.props.meta.anchor === 'none') {
      return div({}, "");
    }
    navClas = clas({
      container: this.props.meta.container === 'false'
    });
    kidsPath = this.props.sein;
    if (this.props.meta.navpath) {
      kidsPath = this.props.meta.navpath;
    }
    kids = [
      Nav({
        curr: this.props.name,
        dataPath: kidsPath,
        meta: this.props.meta,
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
    return div({
      id: 'head',
      className: navClas
    }, kids);
  }
}));


},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":28,"../utils/util.coffee":30,"./Async.coffee":2,"./BodyComponent.coffee":3,"./DpadComponent.coffee":7,"./Reactify.coffee":18,"./SibsComponent.coffee":22,"classnames":31}],15:[function(require,module,exports){
var a, li, nav, recl, ref, rele, ul;

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, nav = ref.nav, ul = ref.ul, li = ref.li, a = ref.a;

module.exports = recl({
  getInitialState: function() {
    return {
      loaded: urb.ship != null
    };
  },
  componentDidMount: function() {
    return urb.init((function(_this) {
      return function() {
        return _this.setState({
          'loaded': 'loaded'
        });
      };
    })(this));
  },
  render: function() {
    if ((urb.user == null) || urb.user !== urb.ship) {
      return nav({
        className: "navbar panel"
      }, [
        ul({
          className: "nav navbar-nav"
        }, [
          li({
            className: 'nav-item pull-right'
          }, a({
            href: "/~~"
          }, "Log in"))
        ])
      ]);
    } else {
      return nav({
        className: "navbar panel"
      }, [
        ul({
          className: "nav navbar-nav"
        }, [
          li({
            className: "nav-item"
          }, a({
            href: "/~~/talk"
          }, "Talk")), li({
            className: "nav-item"
          }, a({
            href: "/~~/dojo"
          }, "Dojo")), li({
            className: "nav-item"
          }, a({
            href: "/~~/static"
          }, "Static")), li({
            className: 'nav-item pull-right'
          }, a({
            href: "/~/away"
          }, "Log out"))
        ])
      ]);
    }
  }
});


},{}],16:[function(require,module,exports){
var Grid, TreeActions, a, button, code, div, h6, input, load, query, recl, ref1, ref2, rele, span, table, tbody, td, textarea, tr,
  slice = [].slice;

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

rele = React.createElement;

ref1 = React.DOM, div = ref1.div, textarea = ref1.textarea, button = ref1.button, input = ref1.input, a = ref1.a, h6 = ref1.h6, code = ref1.code, span = ref1.span;

ref2 = React.DOM, table = ref2.table, tbody = ref2.tbody, tr = ref2.tr, td = ref2.td;

Grid = function() {
  var _td, _tr, props, rows;
  props = arguments[0], rows = 2 <= arguments.length ? slice.call(arguments, 1) : [];
  _td = function(x) {
    return div({
      className: "td"
    }, x);
  };
  _tr = function(x) {
    if (x != null) {
      return div.apply(null, [{
        className: "tr"
      }].concat(slice.call(x.map(_td))));
    }
  };
  return div.apply(null, [props].concat(slice.call(rows.map(_tr))));
};

module.exports = query({
  plan: 'j',
  beak: 't',
  path: 't'
}, recl({
  displayName: "Plan",
  getInitialState: function() {
    return {
      edit: false,
      plan: this.props.plan,
      focus: null,
      loaded: urb.ship != null
    };
  },
  componentDidMount: function() {
    return urb.init((function(_this) {
      return function() {
        return _this.setState({
          'loaded': 'loaded'
        });
      };
    })(this));
  },
  componentWillReceiveProps: function(props) {
    if (_.isEqual(this.props.plan, this.state.plan)) {
      return this.setState({
        plan: props.plan
      });
    }
  },
  refInput: function(ref) {
    return (function(_this) {
      return function(node) {
        _this[ref] = node;
        if (ref === _this.state.focus) {
          return node != null ? node.focus() : void 0;
        }
      };
    })(this);
  },
  saveInfo: function() {
    var plan, ref3;
    plan = {
      who: this.who.value,
      loc: this.loc.value,
      acc: (ref3 = this.props.plan) != null ? ref3.acc : void 0
    };
    if (!_.isEqual(plan, this.state.plan)) {
      TreeActions.setPlanInfo(plan);
      this.setState({
        plan: plan
      });
    }
    return this.setState({
      edit: false,
      focus: null
    });
  },
  render: function() {
    var acc, beak, editButton, editable, issuedBy, key, loc, path, ref3, ref4, ref5, url, usr, who;
    if (!this.state.loaded) {
      return div({
        className: "plan"
      }, "Loading authentication info");
    }
    ref3 = this.props, beak = ref3.beak, path = ref3.path;
    ref5 = (ref4 = this.state.plan) != null ? ref4 : {}, acc = ref5.acc, loc = ref5.loc, who = ref5.who;
    issuedBy = urb.sein !== urb.ship ? "~" + urb.sein : "self";
    if (urb.user !== urb.ship) {
      editButton = null;
      editable = function(ref, val, placeholder) {
        return val != null ? val : placeholder;
      };
    } else if (this.state.edit) {
      editButton = button({
        className: 'edit',
        onClick: (function(_this) {
          return function() {
            return _this.saveInfo();
          };
        })(this)
      }, "Save");
      editable = (function(_this) {
        return function(ref, val, placeholder) {
          return input({
            placeholder: placeholder,
            defaultValue: val,
            ref: _this.refInput(ref),
            onKeyDown: function(arg) {
              var keyCode;
              keyCode = arg.keyCode;
              if (keyCode === 13) {
                return _this.saveInfo();
              }
            }
          });
        };
      })(this);
    } else {
      editButton = button({
        className: 'edit',
        onClick: (function(_this) {
          return function() {
            return _this.setState({
              edit: true
            });
          };
        })(this)
      }, "Edit");
      editable = (function(_this) {
        return function(ref, val, placeholder) {
          var ref6, ref7;
          return span({
            onClick: function() {
              return _this.setState({
                edit: true,
                focus: ref
              });
            }
          }, val != null ? val : placeholder, ((ref6 = _this.props.plan) != null ? ref6[ref] : void 0) !== ((ref7 = _this.state.plan) != null ? ref7[ref] : void 0) ? rele(load, {}) : void 0);
        };
      })(this);
    }
    return div({
      className: "plan"
    }, div({
      className: "home"
    }, ""), div({
      className: "mono"
    }, "~" + urb.ship), (who != null) || this.state.edit ? h6({}, editable('who', who, "Sun Tzu")) : void 0, Grid({
      className: "grid"
    }, ["Location:", editable('loc', loc, "Unknown")], [
      "Issued by:", a({
        href: "//" + urb.sein + ".urbit.org"
      }, issuedBy)
    ], [
      "Immutable link:", a({
        href: beak + "/web" + path
      }, beak)
    ], !_.isEmpty(acc) ? [
      "Connected to:", div({}, (function() {
        var ref6, results;
        results = [];
        for (key in acc) {
          ref6 = acc[key], usr = ref6.usr, url = ref6.url;
          results.push(div({
            key: key,
            className: 'service'
          }, url == null ? key + "/" + usr : a({
            href: url
          }, key + "/" + usr)));
        }
        return results;
      })())
    ] : void 0), editButton);
  }
}));


},{"../actions/TreeActions.coffee":1,"./Async.coffee":2,"./LoadComponent.coffee":12}],17:[function(require,module,exports){
var DEFER_USER, Ship, TreeActions, a, clas, code, div, form, h2, img, input, load, p, query, reactify, recl, ref, rele, textarea, util;

clas = require('classnames');

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeActions = require('../actions/TreeActions.coffee');

util = require('../utils/util.coffee');

Ship = require('./ShipComponent.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, p = ref.p, h2 = ref.h2, img = ref.img, a = ref.a, form = ref.form, textarea = ref.textarea, input = ref.input, code = ref.code;

DEFER_USER = false;

module.exports = query({
  comt: 'j',
  path: 't',
  spur: 't'
}, recl({
  displayName: "Post",
  getInitialState: function() {
    var ref1;
    return {
      loading: null,
      value: "",
      user: (ref1 = urb.user) != null ? ref1 : ""
    };
  },
  componentDidMount: function() {
    if (!DEFER_USER) {
      return urb.init((function(_this) {
        return function() {
          return _this.setState({
            user: urb.user
          });
        };
      })(this));
    }
  },
  componentDidUpdate: function(_props) {
    var ref1;
    if (urb.user && !this.state.user) {
      this.setState({
        user: (ref1 = urb.user) != null ? ref1 : ""
      });
    }
    if (this.props.comt.length > _props.comt.length) {
      return this.setState({
        loading: null
      });
    }
  },
  onSubmit: function(e) {
    var comment, path, title;
    this.setState({
      loading: true
    });
    title = this.refs["in"].title.value;
    comment = this.refs["in"].comment.value;
    path = this.props.path || "/";
    TreeActions.addPost(path, this.props.spur, title, comment);
    return e.preventDefault();
  },
  onChange: function(e) {
    return this.setState({
      value: e.target.value
    });
  },
  render: function() {
    var bodyTextArea, postButton, titleInput;
    titleInput = input({
      disabled: this.state.loading ? "true" : void 0,
      type: "text",
      name: "title",
      placeholder: "Title"
    });
    bodyTextArea = textarea({
      disabled: this.state.loading ? "true" : void 0,
      type: "text",
      name: "comment",
      value: this.state.value,
      onChange: this.onChange
    });
    postButton = input({
      disabled: this.state.loading ? "true" : void 0,
      type: "submit",
      value: "Post",
      className: "btn btn-primary"
    });
    return div({}, div({
      className: "add-post"
    }, form({
      ref: "in",
      onSubmit: this.onSubmit
    }, rele(Ship, {
      ship: this.state.user
    }), titleInput, bodyTextArea, postButton)));
  }
}));


},{"../actions/TreeActions.coffee":1,"../utils/util.coffee":30,"./Async.coffee":2,"./LoadComponent.coffee":12,"./Reactify.coffee":18,"./ShipComponent.coffee":21,"classnames":31}],18:[function(require,module,exports){
var DynamicVirtual, TreeStore, Virtual, div, load, name, reactify, recl, ref, rele, span, walk;

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, span = ref.span;

load = React.createFactory(require('./LoadComponent.coffee'));

TreeStore = require('../stores/TreeStore.coffee');

name = function(displayName, component) {
  return _.extend(component, {
    displayName: displayName
  });
};

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

DynamicVirtual = recl({
  displayName: "DynamicVirtual",
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
    return Virtual(_.extend({}, this.props, {
      components: this.state.components
    }));
  }
});

Virtual = name("Virtual", function(arg) {
  var basePath, components, manx;
  manx = arg.manx, components = arg.components, basePath = arg.basePath;
  return walk(manx, function() {
    return load({}, "");
  }, function(str) {
    return str;
  }, function(arg1, key) {
    var c, e, ga, gn, props, ref1;
    gn = arg1.gn, ga = arg1.ga, c = arg1.c;
    props = {
      key: key
    };
    if (ga != null ? ga.style : void 0) {
      try {
        ga.style = eval("(" + ga.style + ")");
      } catch (error) {
        e = error;
        ga.style = ga.style;
      }
    }
    if (components[gn]) {
      props.basePath = basePath;
    }
    return rele((ref1 = components[gn]) != null ? ref1 : gn, _.extend(props, ga), c.length ? c : void 0);
  });
});

reactify = function(manx, key, arg) {
  var basePath, components, ref1;
  ref1 = arg != null ? arg : {}, basePath = ref1.basePath, components = ref1.components;
  if (components != null) {
    return rele(Virtual, {
      manx: manx,
      key: key,
      basePath: basePath,
      components: components
    });
  } else {
    return rele(DynamicVirtual, {
      manx: manx,
      key: key,
      basePath: basePath
    });
  }
};

module.exports = _.extend(reactify, {
  walk: walk,
  Virtual: Virtual
});


},{"../stores/TreeStore.coffee":28,"./LoadComponent.coffee":12}],19:[function(require,module,exports){
var TreeActions, appendNext, recl, rele, waitingScripts;

recl = React.createClass;

rele = React.createElement;

TreeActions = require('../actions/TreeActions.coffee');

waitingScripts = null;

appendNext = function() {
  if (waitingScripts == null) {
    return;
  }
  if (waitingScripts.length === 0) {
    return waitingScripts = null;
  } else {
    return document.body.appendChild(waitingScripts.shift());
  }
};

module.exports = recl({
  displayName: "Script",
  componentDidMount: function() {
    var s;
    s = document.createElement('script');
    _.assign(s, this.props);
    TreeActions.registerScriptElement(s);
    s.onload = appendNext;
    this.js = s;
    if (waitingScripts != null) {
      return waitingScripts.push(s);
    } else {
      waitingScripts = [s];
      return appendNext();
    }
  },
  componentWillUnmount: function() {
    if (this.js.parentNode === document.body) {
      return document.body.removeChild(this.js);
    }
  },
  render: function() {
    return rele("script", this.props);
  }
});


},{"../actions/TreeActions.coffee":1}],20:[function(require,module,exports){
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


},{"./Async.coffee":2,"./Reactify.coffee":18}],21:[function(require,module,exports){
var div, recl;

recl = React.createClass;

div = React.DOM.div;

module.exports = recl({
  render: function() {
    var attr;
    attr = {
      "data-alias": "~" + window.tree.util.shortShip(this.props.ship),
      className: 'ship'
    };
    return div(attr, "~", this.props.ship);
  }
});


},{}],22:[function(require,module,exports){
var a, clas, li, query, reactify, recl, ref, ul, util;

util = require('../utils/util.coffee');

clas = require('classnames');

reactify = require('./Reactify.coffee');

query = require('./Async.coffee');

recl = React.createClass;

ref = React.DOM, ul = ref.ul, li = ref.li, a = ref.a;

module.exports = query({
  kids: {
    head: 'r',
    meta: 'j',
    name: 't',
    path: 't',
    bump: 't'
  }
}, recl({
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
    var kids, navClas;
    kids = util.sortKids(this.props.kids, this.props.meta.navsort);
    navClas = {
      nav: true,
      'col-md-12': this.props.meta.navmode === 'navbar'
    };
    if (this.props.className) {
      navClas[this.props.className] = true;
    }
    navClas = clas(navClas);
    return ul({
      className: navClas
    }, kids.map((function(_this) {
      return function(arg) {
        var className, head, href, meta, name, path, ref1, selected;
        head = arg.head, meta = (ref1 = arg.meta) != null ? ref1 : {}, name = arg.name, path = arg.path;
        selected = name === _this.props.curr;
        if (_this.props.meta.navselect) {
          selected = name === _this.props.meta.navselect;
        }
        href = util.basepath(path);
        head = meta.title;
        if (head == null) {
          head = _this.toText(head);
        }
        head || (head = name);
        className = clas({
          "nav-item": true,
          selected: selected
        });
        if (meta.sibsclass) {
          className += " " + clas(meta.sibsclass.split(","));
        }
        return li({
          className: className,
          key: name
        }, a({
          className: "nav-link",
          href: href,
          onClick: _this.props.closeNav
        }, head));
      };
    })(this)));
  }
}));


},{"../utils/util.coffee":30,"./Async.coffee":2,"./Reactify.coffee":18,"classnames":31}],23:[function(require,module,exports){
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
    var c, comp, ga, gn;
    gn = arg.gn, ga = arg.ga, c = arg.c;
    if (this.props.match) {
      comp = gn === this.props.match;
    } else {
      comp = gn && gn[0] === 'h' && parseInt(gn[1]) !== (0/0);
    }
    if (comp) {
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
    var contents, i, len, ref, ref1, v;
    if (this.props.body.c) {
      ref = this.props.body.c;
      for (i = 0, len = ref.length; i < len; i++) {
        v = ref[i];
        if (v.gn === 'div' && ((ref1 = v.ga) != null ? ref1.id : void 0) === "toc") {
          contents = [{
              gn: "h1",
              ga: {
                className: "t"
              },
              c: ["Table of contents"]
            }].concat(slice.call(_.filter(v.c.map(this.collectHeader))));
          if (this.props.noHeader) {
            contents.shift();
          }
          return {
            gn: "div",
            ga: {
              className: "toc"
            },
            c: contents
          };
        }
      }
    }
  },
  render: function() {
    return reactify(this.parseHeaders());
  }
}));


},{"./Async.coffee":2,"./Reactify.coffee":18}],24:[function(require,module,exports){
var body, clas, div, head, query, recf, recl;

query = require('./Async.coffee');

clas = require('classnames');

recf = React.createFactory;

recl = React.createClass;

head = recf(require('./NavComponent.coffee'));

body = recf(require('./BodyComponent.coffee'));

div = React.DOM.div;

module.exports = query({
  body: 'r',
  name: 't',
  path: 't',
  meta: 'j',
  sein: 't'
}, recl({
  displayName: "Tree",
  render: function() {
    var treeClas;
    treeClas = clas({
      container: this.props.meta.container !== 'false'
    });
    return div({
      className: treeClas
    }, [
      head({
        key: 'head-container'
      }, ""), body({
        key: 'body-container'
      }, "")
    ]);
  }
}));


},{"./Async.coffee":2,"./BodyComponent.coffee":3,"./NavComponent.coffee":14,"classnames":31}],25:[function(require,module,exports){
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


},{}],26:[function(require,module,exports){
var rend;

rend = ReactDOM.render;

$(function() {
  var frag, main, util;
  util = require('./utils/util.coffee');
  window.tree.util = util;
  require('./utils/scroll.coffee');
  if (document.location.pathname.substr(-1) !== "/") {
    history.replaceState({}, "", document.location.pathname + "/" + document.location.search + document.location.hash);
  }
  window.tree.actions = require('./actions/TreeActions.coffee');
  window.tree.actions.addVirtual(require('./components/Components.coffee'));
  frag = util.fragpath(window.location.pathname.replace(/\.[^\/]*$/, ''));
  window.tree.actions.setCurr(frag, true);
  window.tree.actions.loadPath(frag, window.tree.data);
  if (window.tree.sein != null) {
    window.tree.actions.loadSein(frag, window.tree.sein);
  }
  window.urb.ondataupdate = function(dep) {
    var dat;
    for (dat in window.urb.datadeps) {
      window.urb.dewasp(dat);
    }
    return window.tree.actions.clearData();
  };
  main = React.createFactory(require('./components/TreeComponent.coffee'));
  return rend(main({}, ""), document.getElementById('tree'));
});


},{"./actions/TreeActions.coffee":1,"./components/Components.coffee":6,"./components/TreeComponent.coffee":24,"./utils/scroll.coffee":29,"./utils/util.coffee":30}],27:[function(require,module,exports){
var dedup, pending, util, waspWait;

util = require('../utils/util.coffee');

dedup = {};

pending = {};

waspWait = [];

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
    pending[url] = true;
    return $.get(url, {}, function(data, status, xhr) {
      var dep;
      delete pending[url];
      if (urb.wasp != null) {
        dep = urb.getXHRWasp(xhr);
        urb.sources[dep] = url;
        waspWait.push(dep);
        if (_.isEmpty(pending)) {
          waspWait.map(urb.waspData);
          waspWait = [];
        }
      }
      if (cb) {
        return cb(null, data);
      }
    });
  },
  put: function(data, mark, appl, cb) {
    if (appl == null) {
      appl = /[a-z]*/.exec(mark)[0];
    }
    return urb.init(function() {
      return urb.send(data, {
        mark: mark,
        appl: appl
      }, cb);
    });
  },
  waspElem: function(a) {
    if (urb.wasp != null) {
      return urb.waspElem(a);
    }
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


},{"../utils/util.coffee":30}],28:[function(require,module,exports){
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
  comt: 'j',
  plan: 'j',
  beak: 't',
  spur: 't',
  bump: 't'
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
    }
    if (query.kids) {
      if ((have != null ? have.kids : void 0) === false) {
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
  loadSein: function(arg) {
    var data, path, sein;
    path = arg.path, data = arg.data;
    sein = this.getPare(path);
    if (sein != null) {
      return this.loadPath({
        path: sein,
        data: data
      });
    }
  },
  loadPath: function(arg) {
    var data, path;
    path = arg.path, data = arg.data;
    return this.loadValues(this.getTree(path.split('/'), true), path, data);
  },
  loadValues: function(tree, path, data) {
    var _path, k, old, ref, ref1, v;
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
      _path = path;
      if (_path === "/") {
        _path = "";
      }
      this.loadValues(tree[k], _path + "/" + k, v);
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
      if (!sub) {
        continue;
      }
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
  closeNav: function() {
    return _nav.open = false;
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


},{"../dispatcher/Dispatcher.coffee":25,"events":32}],29:[function(require,module,exports){
var TreeActions, scroll;

TreeActions = require('../actions/TreeActions.coffee');

scroll = {
  w: null,
  $d: null,
  $n: null,
  nh: null,
  cs: null,
  ls: null,
  cwh: null,
  lwh: null,
  track: function() {
    this.w = $(window).width();
    this.$n = $('#head');
    this.$d = $('#head .ctrl');
    return this.nh = $('#head .ctrl').outerHeight(true);
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
    if (!((this.$n != null) && (this.$d != null))) {
      return;
    }
    this.cs = $(window).scrollTop();
    this.cwh = window.innerHeight;
    if (this.w > 767) {
      this.clearNav();
    }
    if (this.w < 767) {
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
            top: top
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
        if (this.cwh === this.lwh) {
          if (!this.$n.hasClass('m-up')) {
            this.$n.removeClass('m-down m-fixed').addClass('m-up');
            TreeActions.closeNav();
            $('.menu.open').removeClass('open');
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
              TreeActions.closeNav();
            }
          }
        }
      }
    }
    this.ls = this.cs;
    return this.lwh = this.cwh;
  },
  init: function() {
    setInterval(this.track.bind(this), 200);
    this.ls = $(window).scrollTop();
    this.cs = $(window).scrollTop();
    $(window).on('resize', this.resize.bind(this));
    return $(window).on('scroll', this.scroll.bind(this));
  }
};

scroll.init();

module.exports = scroll;


},{"../actions/TreeActions.coffee":1}],30:[function(require,module,exports){
var _basepath;

_basepath = window.urb.util.basepath("/");

_basepath += (window.location.pathname.replace(window.tree._basepath, "")).split("/")[0];

module.exports = {
  components: {
    ship: require('../components/ShipComponent.coffee')
  },
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
  shortShip: function(ship) {
    var ref;
    if (ship == null) {
      ship = (ref = urb.user) != null ? ref : "";
    }
    if (ship.length <= 13) {
      return ship;
    } else if (ship.length === 27) {
      return ship.slice(14, 20) + "^" + ship.slice(-6);
    } else {
      return ship.slice(0, 6) + "_" + ship.slice(-6);
    }
  },
  dateFromAtom: function(date) {
    var __, d, day, hor, min, mon, ref, sec, yer;
    ref = date.slice(1).split("."), yer = ref[0], mon = ref[1], day = ref[2], __ = ref[3], hor = ref[4], min = ref[5], sec = ref[6];
    if (day != null) {
      d = new Date();
      d.setYear(yer);
      d.setMonth(mon - 1);
      d.setDate(day);
    }
    if (hor != null) {
      d.setHours(hor);
      d.setMinutes(min);
      d.setSeconds(sec);
    }
    return d;
  },
  getKeys: function(kids, sortBy) {
    return _.map(this.sortKids(kids, sortBy), 'name');
  },
  sortKids: function(kids, sortBy) {
    var _k, _kids, date, i, j, k, len, len1, ref, ref1, ref2, ref3, results, results1, v;
    if (sortBy == null) {
      sortBy = null;
    }
    kids = _.filter(kids, function(arg) {
      var meta;
      meta = arg.meta;
      return !(meta != null ? meta.hide : void 0);
    });
    switch (sortBy) {
      case 'bump':
        return _.sortBy(kids, (function(_this) {
          return function(arg) {
            var bump, meta, name;
            bump = arg.bump, meta = arg.meta, name = arg.name;
            return _this.dateFromAtom(bump || (meta != null ? meta.date : void 0) || name);
          };
        })(this)).reverse();
      case 'date':
        _kids = [];
        for (k in kids) {
          v = kids[k];
          if (((ref = v.meta) != null ? ref.date : void 0) == null) {
            return _.sortBy(kids, 'name');
          }
          date = this.dateFromAtom(v.meta.date);
          if (date == null) {
            return _.sortBy(kids, 'name');
          }
          _k = Number(new Date(date));
          _kids[_k] = v;
        }
        ref1 = _.keys(_kids).sort().reverse();
        results = [];
        for (i = 0, len = ref1.length; i < len; i++) {
          k = ref1[i];
          results.push(_kids[k]);
        }
        return results;
        break;
      case null:
        _kids = [];
        for (k in kids) {
          v = kids[k];
          if (((ref2 = v.meta) != null ? ref2.sort : void 0) == null) {
            return _.sortBy(kids, 'name');
          }
          _kids[Number(v.meta.sort)] = v;
        }
        ref3 = _.keys(_kids).sort();
        results1 = [];
        for (j = 0, len1 = ref3.length; j < len1; j++) {
          k = ref3[j];
          results1.push(_kids[k]);
        }
        return results1;
        break;
      default:
        throw new Error("Unknown sort: " + sortBy);
    }
  }
};


},{"../components/ShipComponent.coffee":21}],31:[function(require,module,exports){
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

},{}],32:[function(require,module,exports){
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

},{}]},{},[26]);
