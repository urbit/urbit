(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var TreeDispatcher, TreePersistence;

TreeDispatcher = require('../dispatcher/Dispatcher.coffee');

TreePersistence = require('../persistence/TreePersistence.coffee');

module.exports = {
  loadPath: function(path, data) {
    return TreeDispatcher.handleServerAction({
      path: path,
      data: data,
      type: "path-load"
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
  setCurr: function(path) {
    return TreeDispatcher.handleViewAction({
      type: "set-curr",
      path: path
    });
  },
  saveFile: function(spur, mime, cb) {
    return TreePersistence.put(spur, mime, function(err, res) {
      if (err != null) {
        throw err;
      }
      if (cb != null) {
        return cb(res);
      }
    });
  },
  deleteFile: function(spur, cb) {
    return TreePersistence.del(spur, function(err, res) {
      if (err != null) {
        throw err;
      }
      if (cb != null) {
        return cb(res);
      }
    });
  }
};



},{"../dispatcher/Dispatcher.coffee":14,"../persistence/TreePersistence.coffee":20}],2:[function(require,module,exports){
var BodyComponent, CLICK, Links, TreeActions, TreeStore, a, clas, div, query, reactify, recl, ref;

clas = require('classnames');

BodyComponent = React.createFactory(require('./BodyComponent.coffee'));

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

TreeStore = require('../stores/TreeStore.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, a = ref.a;

Links = React.createFactory(query({
  path: 't',
  kids: {
    name: 't',
    head: 'r',
    meta: 'j'
  }
}, recl({
  displayName: "Links",
  render: function() {
    return div({
      className: 'links'
    }, this.props.children, this.renderUp(), this.renderSibs(), this.renderArrows());
  },
  renderUp: function() {
    if (this.props.sein) {
      return div({
        id: "up",
        key: "up"
      }, this.renderArrow("up", this.props.sein));
    }
  },
  renderSibs: function() {
    var keys, style;
    keys = window.tree.util.getKeys(this.props.kids);
    if (keys.indexOf(this.props.curr) !== -1) {
      style = {
        marginTop: -24 * (keys.indexOf(this.props.curr)) + "px"
      };
    }
    return div({
      id: "sibs",
      style: style
    }, keys.map((function(_this) {
      return function(key) {
        var className, data, head, href;
        href = window.tree.basepath(_this.props.path + "/" + key);
        data = _this.props.kids[key];
        if (data.meta) {
          head = data.meta.title;
        }
        if (head == null) {
          head = _this.toText(data.head);
        }
        head || (head = key);
        className = clas({
          active: key === _this.props.curr
        });
        return div({
          className: className,
          key: key
        }, a({
          href: href,
          onClick: _this.props.onClick
        }, head));
      };
    })(this)));
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
  renderArrows: function() {
    var index, keys, next, prev, sein;
    keys = window.tree.util.getKeys(this.props.kids);
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
      if (prev || next) {
        return _.filter([
          div({
            id: "sides",
            key: "sides"
          }, prev ? this.renderArrow("prev", sein + "/" + prev) : void 0, next ? this.renderArrow("next", sein + "/" + next) : void 0)
        ]);
      }
    }
  },
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
  }
}), recl({
  displayName: "Links_loading",
  render: function() {
    return div({
      className: 'links'
    }, this.props.children, this._render());
  },
  _render: function() {
    return div({
      id: "sibs"
    }, div({
      className: "active"
    }, a({}, this.props.curr)));
  }
})));

CLICK = 'a';

module.exports = query({
  sein: 't',
  path: 't',
  name: 't',
  meta: 'j'
}, recl({
  displayName: "Anchor",
  getInitialState: function() {
    return {
      url: window.location.pathname
    };
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
  toggleFocus: function(state) {
    return $(this.getDOMNode()).toggleClass('focus', state);
  },
  componentWillUnmount: function() {
    clearInterval(this.interval);
    return $('body').off('click', CLICK);
  },
  componentDidUpdate: function() {
    return this.setTitle();
  },
  componentDidMount: function() {
    var _this;
    this.setTitle();
    this.interval = setInterval(this.checkURL, 100);
    _this = this;
    return $('body').on('click', CLICK, function(e) {
      var href, id;
      href = $(this).attr('href');
      id = $(this).attr('id');
      if (href && !/^https?:\/\//i.test(href)) {
        e.preventDefault();
        e.stopPropagation();
        if ((href != null ? href[0] : void 0) !== "/") {
          href = (document.location.pathname.replace(/[^\/]*\/?$/, '')) + href;
        }
        _this.goTo(window.tree.fragpath(href));
      }
      if (id) {
        return window.location.hash = id;
      }
    });
  },
  setTitle: function() {
    var ref1, title;
    title = $('#cont h1').first().text() || this.props.name;
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
      history.pushState({}, "", window.tree.basepath(href_parts.join("")));
    }
    if (next !== this.props.path) {
      React.unmountComponentAtNode($('#cont')[0]);
      TreeActions.setCurr(next);
      return React.render(BodyComponent({}, ""), $('#cont')[0]);
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
    this.toggleFocus(false);
    this.reset();
    return this.setPath(path);
  },
  checkURL: function() {
    if (this.state.url !== window.location.pathname) {
      this.reset();
      this.setPath(window.tree.fragpath(window.location.pathname), false);
      if (this.isMounted()) {
        return this.setState({
          url: window.location.pathname
        });
      }
    }
  },
  render: function() {
    var obj;
    if (this.props.meta.anchor === 'none') {
      return div({}, "");
    }
    obj = {
      onMouseOver: this.onMouseOver,
      onMouseOut: this.onMouseOut,
      onClick: this.onClick,
      onTouchStart: this.onTouchStart,
      onTouchEnd: this.onTouchEnd
    };
    if (_.keys(window).indexOf("ontouchstart") !== -1) {
      delete obj.onMouseOver;
      delete obj.onMouseOut;
    }
    return div(obj, Links({
      onClick: this.onClick,
      curr: this.props.name,
      dataPath: this.props.sein,
      sein: this.props.sein
    }));
  }
}), "div");



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":21,"./Async.coffee":3,"./BodyComponent.coffee":4,"./Reactify.coffee":11,"classnames":16}],3:[function(require,module,exports){
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
      return {
        got: TreeStore.fulfill(this.getPath(), queries)
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
      return TreeActions.sendQuery(this.getPath(), this.filterQueries());
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
      return div({}, this.filterQueries() != null ? React.createElement(load, this.props) : (!this.getHashElement() ? setTimeout(this.scrollHash, 0) : void 0, React.createElement(Child, _.extend({}, this.props, this.state.got, {
        ref: "loaded"
      }), this.props.children)));
    }
  });
};



},{"../actions/TreeActions.coffee":1,"../stores/TreeStore.coffee":21,"./LoadComponent.coffee":10}],4:[function(require,module,exports){
var Add, Edit, TreeActions, a, button, clas, codemirror, div, extras, img, input, load, p, pre, query, reactify, recl, ref, rele;

clas = require('classnames');

load = require('./LoadComponent.coffee');

query = require('./Async.coffee');

reactify = require('./Reactify.coffee');

codemirror = require('./CodeMirror.coffee');

TreeActions = require('../actions/TreeActions.coffee');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, input = ref.input, div = ref.div, pre = ref.pre, p = ref.p, img = ref.img, a = ref.a, button = ref.button;

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
        className: "logo"
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
        keys = window.tree.util.getKeys(this.props.kids);
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
            }, [
              a({
                href: this.props.path + "/" + next.name
              }, "Next: " + next.meta.title)
            ]);
          }
        }
      }
      return div({}, "");
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

Edit = query({
  spur: 't',
  mime: 'm'
}, recl({
  displayName: "Edit",
  doneEditing: function() {
    var txt;
    txt = $(this.getDOMNode()).find('.CodeMirror')[0].CodeMirror.getValue();
    if (txt === this.props.mime.octs) {
      return this.props.unsetEdit(false);
    }
    TreeActions.saveFile(this.props.spur, txt, (function(_this) {
      return function() {
        return _this.props.unsetEdit();
      };
    })(this));
    return this.props.setPending();
  },
  render: function() {
    var mite, octs, ref1;
    ref1 = this.props.mime, mite = ref1.mite, octs = ref1.octs;
    return rele(codemirror, {
      value: octs,
      mode: mite,
      readOnly: false,
      autofocus: true
    });
  }
}));

Add = query({
  spur: 't',
  path: 't'
}, recl({
  displayName: "Add",
  getInitialState: function() {
    return {
      input: false
    };
  },
  onClick: function() {
    return this.setState({
      input: true
    });
  },
  componentDidUpdate: function() {
    if (this.state.input) {
      return $(this.getDOMNode()).focus();
    }
  },
  render: function() {
    if (!this.state.input) {
      return button({
        onClick: this.onClick
      }, "Add");
    } else {
      return input({
        type: "text",
        onKeyDown: (function(_this) {
          return function(e) {
            var escp, newPath, newSpur, value;
            if (13 === e.keyCode) {
              value = _this.getDOMNode().value;
              escp = value.toLowerCase().replace(/[^a-z0-9._~-]+/g, '-');
              newPath = _this.props.path + "/" + escp;
              newSpur = "/" + escp + _this.props.spur;
              history.pushState({}, "", window.tree.basepath(newPath + "#edit"));
              urb.onupdate = function() {};
              TreeActions.saveFile(newSpur, '# ' + value, function() {});
              TreeActions.loadPath(newPath, {
                spur: newSpur,
                meta: {},
                mime: {
                  mite: "text/x-markdown",
                  octs: '# ' + value
                },
                body: {
                  gn: "div",
                  ga: {},
                  c: [
                    {
                      gn: "h1",
                      ga: {},
                      c: [value]
                    }
                  ]
                }
              });
              return _this.setState({
                input: false
              });
            }
          };
        })(this)
      });
    }
  }
}));

module.exports = query({
  body: 'r',
  name: 't',
  path: 't',
  meta: 'j',
  sein: 't'
}, recl({
  displayName: "Body",
  isOwn: function() {
    return (urb.user != null) && urb.user === urb.ship;
  },
  getInitialState: function() {
    return {
      edit: this.isOwn() && document.location.hash === "#edit"
    };
  },
  pauseWasp: function() {
    this.urb_onupdate = urb.onupdate;
    return urb.onupdate = (function(_this) {
      return function() {
        return _this.urb_updated = arguments;
      };
    })(this);
  },
  resumeWasp: function() {
    if (this.urb_onupdate) {
      urb.onupdate = this.urb_onupdate;
      delete this.urb_onupdate;
      if (this.urb_updated) {
        return urb.onupdate.apply(urb, this.urb_updated);
      }
    }
  },
  setPending: function() {
    return this.setState({
      edit: "pending"
    });
  },
  setEdit: function() {
    this.hash = document.location.hash;
    document.location.hash = "#edit";
    this.setState({
      edit: true
    });
    return this.pauseWasp();
  },
  unsetEdit: function(mod) {
    var ref1;
    document.location.hash = (ref1 = this.hash) != null ? ref1 : "";
    if (mod !== false) {
      return document.location.reload();
    } else {
      this.resumeWasp();
      return this.setState({
        edit: false
      });
    }
  },
  doDelete: function() {
    TreeActions.deleteFile(this.props.spur, (function(_this) {
      return function() {
        return _this.unsetEdit();
      };
    })(this));
    return this.setState({
      edit: "gone"
    });
  },
  render: function() {
    var body, className, editButton, extra, onClick, own, ref1;
    className = clas((ref1 = this.props.meta.layout) != null ? ref1.split(',') : void 0);
    own = this.isOwn();
    extra = (function(_this) {
      return function(name, props) {
        if (props == null) {
          props = {};
        }
        if (_this.props.meta[name] != null) {
          return rele(extras[name], props);
        }
      };
    })(this);
    switch (this.state.edit) {
      case false:
        body = reactify(this.props.body);
        editButton = button({
          onClick: (function(_this) {
            return function() {
              return _this.setEdit();
            };
          })(this)
        }, "Edit");
        break;
      case "pending":
      case "gone":
        body = div({}, rele(load, {}));
        editButton = button({
          onClick: (function(_this) {
            return function() {
              return _this.setEdit();
            };
          })(this)
        }, "Edit");
        break;
      case true:
        body = rele(Edit, {
          ref: "editor",
          setPending: this.setPending,
          unsetEdit: this.unsetEdit
        });
        onClick = (function(_this) {
          return function() {
            var loaded;
            loaded = _this.refs.editor.refs.loaded;
            if (loaded != null) {
              return loaded.doneEditing();
            } else {
              return _this.unsetEdit(false);
            }
          };
        })(this);
        editButton = button({
          onClick: onClick
        }, "Done");
    }
    return div({
      id: 'body',
      key: "body" + this.props.path,
      className: className
    }, extra('spam'), extra('logo', {
      color: this.props.meta.logo
    }), own ? editButton : void 0, own ? button({
      onClick: (function(_this) {
        return function() {
          return _this.doDelete();
        };
      })(this)
    }, "Delete") : void 0, body, extra('next', {
      dataPath: this.props.sein,
      curr: this.props.name
    }), own ? rele(Add, {}) : void 0, extra('footer'));
  }
}));



},{"../actions/TreeActions.coffee":1,"./Async.coffee":3,"./CodeMirror.coffee":5,"./LoadComponent.coffee":10,"./Reactify.coffee":11,"classnames":16}],5:[function(require,module,exports){
var div, recl, ref, textarea;

recl = React.createClass;

ref = React.DOM, div = ref.div, textarea = ref.textarea;

module.exports = recl({
  render: function() {
    return div({}, textarea({
      ref: 'ed',
      defaultValue: this.props.value
    }));
  },
  componentDidMount: function() {
    return CodeMirror.fromTextArea(this.refs.ed.getDOMNode(), _.extend({
      readOnly: true,
      lineNumbers: true
    }, this.props));
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
  lost: recl({
    render: function() {
      return div({}, "<lost(", this.props.children, ")>");
    }
  })
};



},{"./CodeMirror.coffee":5,"./EmailComponent.coffee":7,"./KidsComponent.coffee":8,"./ListComponent.coffee":9,"./SearchComponent.coffee":12,"./TocComponent.coffee":13}],7:[function(require,module,exports){
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



},{"./Reactify.coffee":11}],8:[function(require,module,exports){
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



},{"./Async.coffee":3,"./Reactify.coffee":11}],9:[function(require,module,exports){
var a, clas, div, h1, li, pre, query, reactify, recl, ref, span, ul;

clas = require('classnames');

reactify = require('./Reactify.coffee');

query = require('./Async.coffee');

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
    });
    kids = this.renderList();
    if (!(kids.length === 0 && (this.props.is404 != null))) {
      return ul({
        className: k
      }, kids);
    }
    return div({
      className: k
    }, h1({
      className: 'error'
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
      href = window.tree.basepath(path);
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



},{"./Async.coffee":3,"./Reactify.coffee":11,"classnames":16}],10:[function(require,module,exports){
var div, input, recl, ref, textarea;

recl = React.createClass;

ref = React.DOM, div = ref.div, input = ref.input, textarea = ref.textarea;

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
      className: "loading"
    }, div({
      className: "spin state-" + this.state.anim
    }, ""));
  }
});



},{}],11:[function(require,module,exports){
var Virtual, div, load, reactify, recl, ref, rele, span, walk;

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, span = ref.span;

load = React.createFactory(require('./LoadComponent.coffee'));

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
  render: function() {
    var components;
    components = window.tree.components;
    return walk(this.props.manx, function() {
      return load({}, "");
    }, function(str) {
      return str;
    }, function(arg, key) {
      var c, ga, gn, ref1;
      gn = arg.gn, ga = arg.ga, c = arg.c;
      return rele((ref1 = components[gn]) != null ? ref1 : gn, _.extend({
        key: key
      }, ga), c);
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



},{"./LoadComponent.coffee":10}],12:[function(require,module,exports){
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



},{"./Async.coffee":3,"./Reactify.coffee":11}],13:[function(require,module,exports){
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



},{"./Async.coffee":3,"./Reactify.coffee":11}],14:[function(require,module,exports){
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



},{"flux":17}],15:[function(require,module,exports){
var rend;

rend = React.render;

$(function() {
  var $body, TreeActions, TreePersistence, body, checkMove, checkScroll, frag, head, po, setSo, so;
  $body = $('body');
  React.initializeTouchEvents(true);
  head = React.createFactory(require('./components/AnchorComponent.coffee'));
  body = React.createFactory(require('./components/BodyComponent.coffee'));
  window.tree.components = require('./components/Components.coffee');
  window.tree._basepath = window.urb.util.basepath("/");
  window.tree._basepath += (window.location.pathname.replace(window.tree._basepath, "")).split("/")[0];
  window.tree.basepath = function(path) {
    var _path, prefix;
    prefix = window.tree._basepath;
    if (prefix === "/") {
      prefix = "";
    }
    if (path[0] !== "/") {
      path = "/" + path;
    }
    _path = prefix + path;
    if (_path.slice(-1) === "/") {
      _path = _path.slice(0, -1);
    }
    return _path;
  };
  window.tree.fragpath = function(path) {
    return path.replace(/\/$/, '').replace(window.tree._basepath, "");
  };
  TreeActions = require('./actions/TreeActions.coffee');
  TreePersistence = require('./persistence/TreePersistence.coffee');
  frag = window.tree.fragpath(window.location.pathname);
  TreeActions.setCurr(frag);
  TreeActions.loadPath(frag, window.tree.body, window.tree.kids);
  rend(head({}, ""), $('#nav')[0]);
  rend(body({}, ""), $('#cont')[0]);
  window.tree.util = {
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
    var db, ds, dx, dy;
    if (po.lm !== null && po.cm !== null) {
      po.cs = $(window).scrollTop();
      db = $(window).height() - (po.cs + window.innerHeight);
      ds = Math.abs(po.cs - po.ls);
      dx = Math.abs(po.cm.x - po.lm.x);
      dy = Math.abs(po.cm.y - po.lm.y);
      $('#nav').toggleClass('moving', dx > 20 || dy > 20 || db < 180);
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



},{"./actions/TreeActions.coffee":1,"./components/AnchorComponent.coffee":2,"./components/BodyComponent.coffee":4,"./components/Components.coffee":6,"./persistence/TreePersistence.coffee":20}],16:[function(require,module,exports){
/*!
  Copyright (c) 2015 Jed Watson.
  Licensed under the MIT License (MIT), see
  http://jedwatson.github.io/classnames
*/
/* global define */

(function () {
	'use strict';

	var hasOwn = {}.hasOwnProperty;

	function classNames () {
		var classes = '';

		for (var i = 0; i < arguments.length; i++) {
			var arg = arguments[i];
			if (!arg) continue;

			var argType = typeof arg;

			if (argType === 'string' || argType === 'number') {
				classes += ' ' + arg;
			} else if (Array.isArray(arg)) {
				classes += ' ' + classNames.apply(null, arg);
			} else if (argType === 'object') {
				for (var key in arg) {
					if (hasOwn.call(arg, key) && arg[key]) {
						classes += ' ' + key;
					}
				}
			}
		}

		return classes.substr(1);
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

},{}],17:[function(require,module,exports){
/**
 * Copyright (c) 2014-2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module.exports.Dispatcher = require('./lib/Dispatcher')

},{"./lib/Dispatcher":18}],18:[function(require,module,exports){
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

},{"./invariant":19}],19:[function(require,module,exports){
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

},{}],20:[function(require,module,exports){
var dedup;

dedup = {};

if (urb.send) {
  urb.appl = 'hood';
}

module.exports = {
  del: function(sup, cb) {
    return urb.send(sup, {
      mark: 'write-wipe'
    }, cb);
  },
  put: function(sup, mime, cb) {
    return urb.send({
      sup: sup,
      mime: mime
    }, {
      mark: 'write-tree'
    }, cb);
  },
  get: function(path, query, cb) {
    var url;
    if (query == null) {
      query = "no-query";
    }
    url = (window.tree.basepath(path)) + ".json?q=" + (this.encode(query));
    if (dedup[url]) {
      return;
    }
    dedup[url] = true;
    return $.get(url, {}, function(data) {
      if (cb) {
        return cb(null, data);
      }
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



},{}],21:[function(require,module,exports){
var EventEmitter, MessageDispatcher, QUERIES, TreeStore, _curr, _data, _tree, clog;

EventEmitter = require('events').EventEmitter;

MessageDispatcher = require('../dispatcher/Dispatcher.coffee');

clog = console.log.bind(console);

_tree = {};

_data = {};

_curr = "";

QUERIES = {
  body: 'r',
  head: 'r',
  snip: 'r',
  sect: 'j',
  meta: 'j',
  mime: 'm',
  spur: 't'
};

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
  setCurr: function(path) {
    return _curr = path;
  },
  getCurr: function() {
    return _curr;
  },
  loadPath: function(path, data) {
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
  }
});

TreeStore.dispatchToken = MessageDispatcher.register(function(payload) {
  var action;
  action = payload.action;
  switch (action.type) {
    case 'path-load':
      TreeStore.loadPath(action.path, action.data);
      return TreeStore.emitChange();
    case 'set-curr':
      TreeStore.setCurr(action.path);
      return TreeStore.emitChange();
  }
});

module.exports = TreeStore;



},{"../dispatcher/Dispatcher.coffee":14,"events":22}],22:[function(require,module,exports){
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

},{}]},{},[15]);
