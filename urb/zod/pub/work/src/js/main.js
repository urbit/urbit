(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Dispatcher, Persistence;

Dispatcher = require('../dispatcher/Dispatcher.coffee');

Persistence = require('../persistence/Persistence.coffee');

module.exports = {
  newItem: function(index, list) {
    var item;
    item = {
      id: window.util.uuid32(),
      version: 0,
      "date-created": Date.now(),
      "date-modified": Date.now(),
      "date-due": null,
      done: null,
      owner: window.urb.ship,
      status: 'announced',
      tags: [],
      title: '',
      description: '',
      discussion: [],
      audience: [window.util.talk.mainStationPath(window.urb.ship)]
    };
    Persistence.put({
      "new": item
    });
    return Dispatcher.handleViewAction({
      type: 'newItem',
      index: index,
      item: item
    });
  },
  setItem: function(id, version, key, val) {
    var set;
    set = {};
    set[key] = val;
    return Persistence.put({
      old: {
        id: id,
        version: version,
        dif: {
          set: set
        }
      }
    });
  },
  addComment: function(id, version, val) {
    return Persistence.put({
      old: {
        id: id,
        version: version,
        dif: {
          add: {
            comment: val
          }
        }
      }
    });
  },
  setFilter: function(key, val) {
    return Dispatcher.handleViewAction({
      type: 'setFilter',
      key: key,
      val: val
    });
  },
  setSort: function(key, val) {
    return Dispatcher.handleViewAction({
      type: 'setSort',
      key: key,
      val: val
    });
  },
  swapItems: function(to, from) {
    return Dispatcher.handleViewAction({
      type: 'swapItem',
      from: from,
      to: to
    });
  },
  removeItem: function(index, id) {
    Persistence.put({
      old: {
        id: id,
        dif: {
          set: {
            done: true
          }
        }
      }
    });
    return Dispatcher.handleViewAction({
      type: 'removeItem',
      index: index
    });
  },
  addItem: function(index, item) {
    return Dispatcher.handleViewAction({
      type: 'addItem',
      index: index,
      item: item
    });
  },
  listenList: function(type) {
    return Persistence.subscribe(type, function(err, d) {
      var ref, sort, tasks;
      if (d != null) {
        ref = d.data, sort = ref.sort, tasks = ref.tasks;
        return Dispatcher.handleServerAction({
          type: "getData",
          sort: sort,
          tasks: tasks
        });
      }
    });
  }
};


},{"../dispatcher/Dispatcher.coffee":8,"../persistence/Persistence.coffee":14}],2:[function(require,module,exports){
var div, h1, label, rece, recl, ref;

recl = React.createClass;

rece = React.createElement;

ref = [React.DOM.div, React.DOM.h1, React.DOM.label], div = ref[0], h1 = ref[1], label = ref[2];

module.exports = recl({
  _onKeyDown: function(e) {
    if (e.keyCode === 13) {
      e.stopPropagation();
      e.preventDefault();
      return this.change(e);
    }
  },
  _onBlur: function(e) {
    return this.change(e);
  },
  change: function(e) {
    var $t, key, txt;
    $t = $(e.target).closest('.filter');
    txt = $t.find('.input').text().trim();
    key = $t.attr('data-key');
    if (txt.length === 0) {
      txt = null;
    } else {
      switch (key) {
        case 'owner':
          txt = "~" + txt;
          break;
        case 'audience':
          txt = txt.split(" ");
          break;
        case 'tags':
          txt = [txt];
      }
    }
    return this.props.onChange(key, txt);
  },
  fields: [
    {
      filter: 'owned',
      key: 'owner',
      title: 'Owner:'
    }, {
      filter: 'tag',
      key: 'tags',
      title: 'Tag:'
    }, {
      filter: 'channel',
      key: 'audience',
      title: 'Audience:'
    }, {
      filter: 'status',
      key: 'status',
      title: 'Status:'
    }
  ],
  render: function() {
    return div({
      className: 'filters'
    }, this.fields.map((function(_this) {
      return function(arg) {
        var filter, key, title;
        filter = arg.filter, key = arg.key, title = arg.title;
        return div({
          key: key,
          'data-key': key,
          className: filter + " filter ib"
        }, [
          label({}, title), div({
            contentEditable: true,
            className: 'input ib',
            onKeyDown: _this._onKeyDown,
            onBlur: _this._onBlur
          }, _this.props.filters[filter])
        ]);
      };
    })(this)));
  }
});


},{}],3:[function(require,module,exports){
var WorkActions, div, recl, ref, textarea;

recl = React.createClass;

ref = [React.DOM.div, React.DOM.textarea], div = ref[0], textarea = ref[1];

WorkActions = require('../actions/WorkActions.coffee');

module.exports = recl({
  _dragStart: function(e) {
    var $t;
    $t = $(e.target);
    this.dragged = $t.closest('.item');
    e.dataTransfer.effectAllowed = 'move';
    e.dataTransfer.setData('text/html', e.currentTarget);
    return this.props._dragStart(e, this);
  },
  _dragEnd: function(e) {
    return this.props._dragEnd(e, this);
  },
  _keyDown: function(e) {
    var kc;
    this.props._keyDown(e, this);
    kc = e.keyCode;
    switch (kc) {
      case 9:
        if (this.state.expand === false) {
          this.setState({
            expand: true
          });
        }
        break;
      case 27:
        this.setState({
          expand: false
        });
    }
    if ((kc === 9 && this.state.expand === false) || (kc === 27)) {
      e.preventDefault();
    }
  },
  getVal: function($el, key) {
    var a, d;
    if ($el[0].tagName === 'TEXTAREA') {
      return $el.val();
    } else {
      if (key === 'date-due') {
        d = $el.text().slice(1).replace(/\./g, "-");
        if (d.length < 8) {
          return NaN;
        }
        return new Date(d).valueOf();
      }
      if (key === 'tags') {
        return $el.text().trim().split(" ");
      }
      if (key === 'audience') {
        a = $el.text().trim().split(" ");
        a = a.map(function(_a) {
          return "~" + _a;
        });
        return a;
      }
      return $el.text();
    }
  },
  compareVal: function(l, n, key) {
    if (key === 'tags' || key === 'audience') {
      return _.xor(l, n).length > 0;
    }
    if (key === 'date-due') {
      return l !== new Date(n);
    }
    return l !== n;
  },
  validateField: function($t, id, key, val) {
    var i, valid;
    valid = 1;
    if (key === 'date-due') {
      if (isNaN(val)) {
        valid = 0;
      }
    }
    if (key === 'audience') {
      i = _.filter(val, function(a) {
        if (a[0] !== "~") {
          return 0;
        }
        if (a.split("/").length < 2) {
          return 0;
        }
        if (a.split("/")[0].length < 3 || a.split("/")[1].length < 3) {
          return 0;
        }
        return 1;
      });
      if (i.length !== val.length) {
        valid = 0;
      }
    }
    return valid;
  },
  _keyUp: function(e) {
    var $t, id, key, val, ver;
    $t = $(e.target).closest('.field');
    id = $t.closest('.item').attr('data-id');
    key = $t.attr('data-key');
    val = this.getVal($t.find('.input'), key);
    if (this.compareVal(this.props.item[key], val, key)) {
      if (!this.validateField($t, id, key, val)) {
        $t.addClass('invalid');
        return;
      }
      $t.removeClass('invalid');
      if (this.to) {
        clearTimeout(this.to);
      }
      ver = this.props.item.version;
      return this.to = setTimeout(function() {
        return WorkActions.setItem(id, ver, key, val);
      }, 1000);
    }
  },
  _focus: function(e) {
    return this.props._focus(e, this);
  },
  _markDone: function(e) {
    var id;
    id = $(e.target).closest('.item').attr('data-id');
    return WorkActions.setItem(id, this.props.item.version, 'done', true);
  },
  _changeStatus: function(e) {
    if (this.props.item.status === 'released') {
      return;
    }
    if (this.props.item.status === 'accepted' && this.props.item.owner.slice(1) !== window.urb.ship) {
      return;
    }
    return WorkActions.changeStatus(this.props.item.id);
  },
  _submitComment: function(e) {
    var $t, id, val;
    $t = $(e.target).closest('.item');
    id = $t.attr('data-id');
    val = $t.find('.comment .input').text();
    return WorkActions.addComment(id, this.props.item.version, val);
  },
  formatDate: function(d) {
    if (d === null) {
      return "";
    }
    return "~" + (d.getFullYear()) + "." + (d.getMonth() + 1) + "." + (d.getDate());
  },
  formatOwner: function(o) {
    if (o === null) {
      return "";
    }
    return o.replace(/\~/g, "");
  },
  formatAudience: function(a) {
    return this.formatOwner(a.join(" "));
  },
  getInitialState: function() {
    return {
      expand: false
    };
  },
  render: function() {
    var action, itemClass;
    itemClass = 'item';
    if (this.state.expand) {
      itemClass += ' expand';
    }
    action = "";
    if (this.props.item.status === 'announce') {
      action = "claim";
    }
    if (this.props.item.status === 'accepted' && this.props.item.owner.slice(1) === window.urb.ship) {
      action = "release";
    }
    return div({
      className: itemClass,
      draggable: true,
      'data-id': this.props.item.id,
      'data-index': this.props.index,
      onDragStart: this._dragStart,
      onDragEnd: this._dragEnd
    }, [
      div({
        className: 'header'
      }, [
        div({
          className: 'owner ib ' + this.props.item.status,
          'data-key': 'owner'
        }, this.props.item.owner.slice(1)), div({
          className: 'status ib action-' + (action.length > 0),
          'data-key': 'status',
          onClick: this._changeStatus
        }, [
          div({
            className: 'label'
          }, this.props.item.status), div({
            className: 'action a'
          }, action)
        ]), div({
          className: 'audience field ib',
          'data-key': 'audience'
        }, [
          div({
            contentEditable: true,
            className: 'input ib'
          }, this.formatAudience(this.props.item.audience))
        ])
      ]), div({
        className: 'sort ib top'
      }, this.props.item.sort), div({
        className: 'done ib',
        onClick: this._markDone
      }, ''), div({
        className: 'title ib top field',
        'data-key': 'title'
      }, [
        div({
          contentEditable: true,
          onFocus: this._focus,
          onKeyDown: this._keyDown,
          onKeyUp: this._keyUp,
          className: 'input ib'
        }, this.props.item.title)
      ]), div({
        className: 'date ib top field',
        'data-key': 'date-due'
      }, [
        div({
          contentEditable: true,
          className: 'input ib',
          onKeyUp: this._keyUp
        }, this.formatDate(this.props.item['date-due']))
      ]), div({
        className: 'tags ib top field',
        'data-key': 'tags'
      }, [
        div({
          contentEditable: true,
          className: 'input ib',
          onKeyUp: this._keyUp
        }, this.props.item.tags.join(" "))
      ]), div({
        className: 'expand ib',
        onClick: (function(_this) {
          return function(e) {
            return _this.setState({
              expand: !_this.state.expand
            });
          };
        })(this)
      }, [
        div({
          className: 'caret left'
        }, "")
      ]), div({
        className: 'description field',
        'data-key': 'description'
      }, [
        textarea({
          className: 'input ib',
          onKeyUp: this._keyUp
        }, this.props.item.description)
      ]), div({
        className: "hr"
      }, ""), div({
        className: "discussion"
      }, [
        div({
          className: "comments"
        }, this.props.item.discussion.map((function(_this) {
          return function(slug) {
            return div({
              className: 'comment'
            }, [
              div({
                className: 'hr2'
              }, ""), div({
                className: 'ship ib'
              }, slug.ship), div({
                className: 'date ib'
              }, _this.formatDate(slug.date)), div({
                className: 'body'
              }, slug.body)
            ]);
          };
        })(this))), div({
          className: 'new comment'
        }, [
          div({
            className: 'hr2'
          }, ""), div({
            className: 'ship ib'
          }, window.urb.ship), div({
            className: 'date ib'
          }, this.formatDate(new Date())), div({
            contentEditable: true,
            className: 'input'
          }, ""), div({
            className: 'submit',
            onClick: this._submitComment
          }, 'Post')
        ])
      ])
    ]);
  }
});


},{"../actions/WorkActions.coffee":1}],4:[function(require,module,exports){
var FilterComponent, ItemComponent, ListeningComponent, SortComponent, WorkActions, WorkStore, div, h1, input, rece, recl, ref, textarea;

recl = React.createClass;

rece = React.createElement;

ref = [React.DOM.div, React.DOM.h1, React.DOM.input, React.DOM.textarea], div = ref[0], h1 = ref[1], input = ref[2], textarea = ref[3];

WorkStore = require('../stores/WorkStore.coffee');

WorkActions = require('../actions/WorkActions.coffee');

ItemComponent = require('./ItemComponent.coffee');

ListeningComponent = require('./ListeningComponent.coffee');

FilterComponent = require('./FilterComponent.coffee');

SortComponent = require('./SortComponent.coffee');

module.exports = recl({
  stateFromStore: function() {
    return {
      list: WorkStore.getList(),
      listening: WorkStore.getListening(),
      sorts: WorkStore.getSorts(),
      filters: WorkStore.getFilters(),
      expand: false
    };
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  alias: function() {
    this.$el = $(this.getDOMNode());
    return this.$items = this.$el.find('.items').children();
  },
  _focus: function(e, i) {
    return this.setState({
      selected: Number(i.props.index)
    });
  },
  _dragStart: function(e, i) {
    return this.dragged = i.dragged;
  },
  _dragEnd: function(e, i) {
    var from, to;
    from = Number(this.dragged.attr('data-index'));
    to = Number(this.over.attr('data-index'));
    if (from < to) {
      to--;
    }
    if (this.drop === 'after') {
      to++;
    }
    WorkActions.swapItems(to, from);
    this.dragged.removeClass('hidden');
    return this.placeholder.remove();
  },
  _dragOver: function(e, i) {
    var $t;
    e.preventDefault();
    $t = $(e.target).closest('.item');
    if ($t.hasClass('placeholder')) {
      return;
    }
    if ($t.length === 0) {
      return;
    }
    this.over = $t;
    if (!this.dragged.hasClass('hidden')) {
      this.dragged.addClass('hidden');
    }
    if ((e.clientY - $t[0].offsetTop) < ($t[0].offsetHeight / 2)) {
      this.drop = 'before';
      return this.placeholder.insertBefore($t);
    } else {
      this.drop = 'after';
      return this.placeholder.insertAfter($t);
    }
  },
  _keyDown: function(e) {
    var ins, kc, last, next;
    kc = e.keyCode;
    switch (kc) {
      case 13:
        if (window.getSelection().getRangeAt(0).endOffset === 0) {
          ins = this.state.selected;
        } else {
          ins = this.state.selected + 1;
          this.setState({
            selected: ins,
            select: true
          });
        }
        WorkActions.newItem(ins);
        break;
      case 8:
        if (window.getSelection().getRangeAt(0).endOffset === 0 && e.target.innerText.length === 0) {
          if (this.state.selected !== 0) {
            this.setState({
              selected: this.state.selected - 1,
              select: "end"
            });
          }
          WorkActions.removeItem(this.state.selected, this.state.list[this.state.selected].id);
          e.preventDefault();
        }
        break;
      case 38:
        last = this.state.selected - 1;
        if (last < 0) {
          last = this.state.list.length - 1;
        }
        this.$items.eq(last).find('.title .input').focus();
        this.setState({
          select: "end"
        });
        break;
      case 40:
        next = this.state.selected + 1;
        if (next === this.state.list.length) {
          next = 0;
        }
        this.$items.eq(next).find('.title .input').focus();
        this.setState({
          select: "end"
        });
    }
    if ((kc === 13) || (kc === 38) || (kc === 40)) {
      return e.preventDefault();
    }
  },
  _changeListening: function() {},
  _changeFilter: function(key, val) {
    return WorkActions.setFilter(key, val);
  },
  _changeSort: function(key, val) {
    return WorkActions.setSort(key, val);
  },
  componentDidMount: function() {
    this.placeholder = $("<div class='item placeholder'><div class='sort'>x</div></div>");
    WorkStore.addChangeListener(this._onChangeStore);
    WorkActions.listenList(this.props.list);
    return this.alias();
  },
  componentDidUpdate: function() {
    var $title, r, s;
    this.alias();
    if (this.state.selected !== void 0 || this.state.select) {
      $title = this.$items.eq(this.state.selected).find('.title .input');
    }
    if (this.state.selected !== void 0 && this.state.select) {
      $title.focus();
    }
    if (this.state.select === "end") {
      r = window.getSelection().getRangeAt(0);
      r.setStart($title[0], 1);
      r.setEnd($title[0], 1);
      s = window.getSelection();
      s.removeAllRanges();
      s.addRange(r);
    }
    if (this.state.select) {
      return this.setState({
        select: false
      });
    }
  },
  render: function() {
    return div({}, [
      div({
        className: 'ctrl'
      }, [
        rece(ListeningComponent, {
          listening: this.state.listening,
          onChange: this._changeListening
        }), div({
          className: 'transforms'
        }, [
          rece(FilterComponent, {
            filters: this.state.filters,
            onChange: this._changeFilter
          }), rece(SortComponent, {
            sorts: this.state.sorts,
            onChange: this._changeSort
          })
        ])
      ]), div({
        className: 'items',
        onDragOver: this._dragOver
      }, [
        _.map(this.state.list, (function(_this) {
          return function(item, index) {
            return rece(ItemComponent, {
              item: item,
              index: index,
              _focus: _this._focus,
              _keyDown: _this._keyDown,
              _dragStart: _this._dragStart,
              _dragEnd: _this._dragEnd
            });
          };
        })(this))
      ])
    ]);
  }
});


},{"../actions/WorkActions.coffee":1,"../stores/WorkStore.coffee":15,"./FilterComponent.coffee":2,"./ItemComponent.coffee":3,"./ListeningComponent.coffee":5,"./SortComponent.coffee":6}],5:[function(require,module,exports){
var div, h1, input, rece, recl, ref, textarea;

recl = React.createClass;

rece = React.createElement;

ref = [React.DOM.div, React.DOM.h1, React.DOM.input, React.DOM.textarea], div = ref[0], h1 = ref[1], input = ref[2], textarea = ref[3];

module.exports = recl({
  render: function() {
    return div({
      className: 'listening'
    }, "");
  }
});


},{}],6:[function(require,module,exports){
var button, div, h1, label, rece, recl, ref;

recl = React.createClass;

rece = React.createElement;

ref = [React.DOM.div, React.DOM.h1, React.DOM.button, React.DOM.label], div = ref[0], h1 = ref[1], button = ref[2], label = ref[3];

module.exports = recl({
  _onClick: function(e) {
    var $t, key, sor;
    $t = $(e.target).closest('.sort');
    key = $t.attr('data-key');
    sor = Number($t.attr('data-state'));
    if (sor === 0) {
      sor = 1;
    } else if (sor === 1) {
      sor = -1;
    } else if (sor === -1) {
      sor = 0;
    }
    return this.props.onChange(key, sor);
  },
  render: function() {
    return div({
      className: 'sorts'
    }, _.map(this.props.sorts, (function(_this) {
      return function(s, k) {
        return button({
          'data-key': k,
          'data-state': s,
          className: "sort s-" + s,
          onClick: _this._onClick
        }, [
          label({}, k), div({
            className: 'caret ib'
          }, '')
        ]);
      };
    })(this)));
  }
});


},{}],7:[function(require,module,exports){
var ListComponent, div, h1, rece, recl, ref;

recl = React.createClass;

rece = React.createElement;

ref = [React.DOM.div, React.DOM.h1], div = ref[0], h1 = ref[1];

ListComponent = require('./ListComponent.coffee');

module.exports = recl({
  render: function() {
    return div({}, [
      h1({
        className: 'leader'
      }, "Work"), rece(ListComponent, {
        list: 'upcoming'
      })
    ]);
  }
});


},{"./ListComponent.coffee":4}],8:[function(require,module,exports){
var Dispatcher;

Dispatcher = require('flux').Dispatcher;

module.exports = _.merge(new Dispatcher(), {
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
var WorkComponent;

WorkComponent = require('./components/WorkComponent.coffee');

window.util = _.extend(window.util || {}, require('./util.coffee'));

$(function() {
  return React.render(React.createElement(WorkComponent), $('#c')[0]);
});


},{"./components/WorkComponent.coffee":7,"./util.coffee":16}],10:[function(require,module,exports){
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
'use strict';

function ToObject(val) {
	if (val == null) {
		throw new TypeError('Object.assign cannot be called with null or undefined');
	}

	return Object(val);
}

module.exports = Object.assign || function (target, source) {
	var pendingException;
	var from;
	var keys;
	var to = ToObject(target);

	for (var s = 1; s < arguments.length; s++) {
		from = arguments[s];
		keys = Object.keys(Object(from));

		for (var i = 0; i < keys.length; i++) {
			try {
				to[keys[i]] = from[keys[i]];
			} catch (err) {
				if (pendingException === undefined) {
					pendingException = err;
				}
			}
		}
	}

	if (pendingException) {
		throw pendingException;
	}

	return to;
};

},{}],14:[function(require,module,exports){
var cache, listeners;

urb.appl = 'work';

listeners = {};

cache = null;

urb.bind("/repo", function(err, dat) {
  var cb, k, results;
  if (err) {
    return document.write(err);
  } else {
    cache = dat;
    results = [];
    for (k in listeners) {
      cb = listeners[k];
      results.push(cb(null, dat));
    }
    return results;
  }
});

module.exports = {
  put: function(update, cb) {
    return urb.send(update, {
      mark: 'work-command'
    }, cb);
  },
  subscribe: function(key, cb) {
    listeners[key] = cb;
    if (cache != null) {
      return cb(null, cache);
    }
  }
};


},{}],15:[function(require,module,exports){
var Dispatcher, EventEmitter, WorkStore, _filters, _list, _listening, _sorts, assign;

EventEmitter = require('events').EventEmitter;

assign = require('object-assign');

Dispatcher = require('../dispatcher/Dispatcher.coffee');

_list = [
  {
    id: "0v0",
    version: 0,
    sort: 0,
    "date-created": new Date('2015-8-18'),
    "date-modified": new Date('2015-8-18'),
    "date-due": new Date('2015-8-18'),
    owner: "~zod",
    audience: ["~doznec/urbit-meta", "~doznec/tlon"],
    status: "announce",
    tags: ['food', 'office'],
    title: 'get groceries',
    description: 'first go out the door, \n then walk down the block.',
    discussion: [
      {
        date: new Date('2015-8-18'),
        ship: "wictuc-folrex",
        body: "Seems like a great idea."
      }
    ]
  }, {
    id: "0v1",
    version: 0,
    sort: 1,
    "date-created": new Date('2015-8-18'),
    "date-modified": new Date('2015-8-18'),
    "date-due": null,
    owner: "~zod",
    audience: ["~doznec/tlon"],
    status: "accepted",
    tags: ['home', 'office'],
    title: 'eat',
    description: 'dont forget about lunch.',
    discussion: []
  }, {
    id: "0v2",
    version: 0,
    sort: 2,
    "date-created": new Date('2015-8-18'),
    "date-modified": new Date('2015-8-18'),
    "date-due": null,
    owner: "~talsur-todres",
    audience: ["~doznec/tlon"],
    status: "accepted",
    tags: ['home'],
    title: 'sleep',
    description: 'go get some sleep.',
    discussion: []
  }
];

_listening = [];

_filters = {
  owner: null,
  tags: null,
  audience: null,
  status: null
};

_sorts = {
  title: 0,
  owner: 0,
  "date-due": 0,
  sort: 0
};

WorkStore = assign({}, EventEmitter.prototype, {
  emitChange: function() {
    return this.emit('change');
  },
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  removeChangeListener: function(cb) {
    return this.removeListener("change", cb);
  },
  getData: function(arg) {
    var _tasks, got, i, id, j, len, sort, tasks;
    sort = arg.sort, tasks = arg.tasks;
    _tasks = _.clone(tasks);
    for (i = j = 0, len = _list.length; j < len; i = ++j) {
      id = _list[i].id;
      if (!(got = _tasks[id])) {
        continue;
      }
      delete _tasks[id];
      _list[i] = this.itemFromData(got, i);
    }
    return sort.map((function(_this) {
      return function(k, index) {
        if (_tasks[k]) {
          return _this.newItem({
            item: _tasks[k],
            index: index
          });
        }
      };
    })(this));
  },
  getList: function(key) {
    var _k, _v, add, c, k, list, v;
    list = [];
    for (k in _list) {
      v = _list[k];
      add = true;
      for (_k in _filters) {
        _v = _filters[_k];
        if (_v === null) {
          continue;
        }
        c = v[_k];
        if (typeof c === 'object') {
          if (_.intersection(c, _v).length === 0) {
            add = false;
          }
        } else {
          if (c !== _v) {
            add = false;
          }
        }
      }
      if (add === true) {
        list.push(v);
      }
    }
    if (_.uniq(_.values(_sorts)).length > 0) {
      for (k in _sorts) {
        v = _sorts[k];
        if (v !== 0) {
          break;
        }
      }
      list = _.sortBy(list, k, k);
      if (v === -1) {
        list.reverse();
      }
    }
    return list;
  },
  getListening: function() {
    return _listening;
  },
  getFilters: function() {
    return _filters;
  },
  setFilter: function(arg) {
    var key, val;
    key = arg.key, val = arg.val;
    return _filters[key] = val;
  },
  getSorts: function() {
    return _sorts;
  },
  setSort: function(arg) {
    var k, key, v, val;
    key = arg.key, val = arg.val;
    for (k in _sorts) {
      v = _sorts[k];
      _sorts[k] = 0;
    }
    return _sorts[key] = val;
  },
  itemFromData: function(item, index) {
    var _item;
    if (index == null) {
      index = 0;
    }
    _item = _.extend({
      sort: index
    }, item);
    _item["date-modified"] = new Date(item["date-modified"]);
    _item["date-created"] = new Date(item["date-created"]);
    if (item["date-due"] != null) {
      _item["date-due"] = new Date(item["date-due"]);
    }
    if (item.done != null) {
      _item.done = new Date(item.done);
    }
    return _item;
  },
  newItem: function(arg) {
    var index, item;
    item = arg.item, index = arg.index;
    return _list.splice(index, 0, this.itemFromData(item, index));
  },
  swapItem: function(arg) {
    var from, to;
    to = arg.to, from = arg.from;
    return _list.splice(to, 0, _list.splice(from, 1)[0]);
  },
  removeItem: function(arg) {
    var index;
    index = arg.index;
    return _list.splice(index, 1);
  }
});

WorkStore.setMaxListeners(100);

WorkStore.dispatchToken = Dispatcher.register(function(p) {
  var a;
  a = p.action;
  if (WorkStore[a.type]) {
    WorkStore[a.type](a);
    return WorkStore.emitChange();
  }
});

module.exports = WorkStore;


},{"../dispatcher/Dispatcher.coffee":8,"events":17,"object-assign":13}],16:[function(require,module,exports){
module.exports = {
  uuid32: function() {
    var i, str, vals;
    vals = (function() {
      var j, results;
      results = [];
      for (i = j = 0; j <= 5; i = ++j) {
        str = Math.ceil(Math.random() * 10000000).toString(32);
        results.push(("00000" + str).substr(-5, 5));
      }
      return results;
    })();
    vals.unshift(Math.ceil(Math.random() * 8));
    return "0v" + vals.join('.');
  },
  getScroll: function() {
    return this.writingPosition = $('#c').outerHeight(true) + $('#c').offset().top - $(window).height();
  },
  setScroll: function() {
    window.util.getScroll();
    return $(window).scrollTop($("#c").height());
  },
  isScrolling: function() {
    if (!window.util.writingPosition) {
      window.util.getScroll();
    }
    return $(window).scrollTop() + $('#writing').outerHeight() < window.util.writingPosition;
  },
  checkScroll: function() {
    if (window.util.isScrolling()) {
      return $('body').addClass('scrolling');
    } else {
      return $('body').removeClass('scrolling');
    }
  },
  talk: {
    mainStations: ["court", "floor", "porch"],
    mainStationPath: function(user) {
      return "~" + user + "/" + (window.util.talk.mainStation(user));
    },
    mainStation: function(user) {
      if (!user) {
        user = window.urb.user;
      }
      switch (user.length) {
        case 3:
          return "court";
        case 6:
          return "floor";
        case 13:
          return "porch";
      }
    }
  }
};


},{}],17:[function(require,module,exports){
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
