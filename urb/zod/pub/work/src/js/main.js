(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Dispatcher, Persistence;

Dispatcher = require('../dispatcher/Dispatcher.coffee');

Persistence = require('../persistence/Persistence.coffee');

module.exports = {
  newItem: function(index, _item) {
    var item, ref, ref1, ref2, ref3, ref4, ref5, ref6, ref7;
    if (_item == null) {
      _item = {};
    }
    item = {
      id: window.util.uuid32(),
      version: 0,
      owner: window.urb.ship,
      date_created: Date.now(),
      date_modified: Date.now(),
      date_due: (ref = _item.date_due) != null ? ref : null,
      done: (ref1 = _item.done) != null ? ref1 : null,
      status: (ref2 = _item.status) != null ? ref2 : 'announced',
      tags: (ref3 = _item.tags) != null ? ref3 : [],
      title: (ref4 = _item.title) != null ? ref4 : '',
      description: (ref5 = _item.description) != null ? ref5 : '',
      discussion: (ref6 = _item.discussion) != null ? ref6 : [],
      audience: (ref7 = _item.audience) != null ? ref7 : [window.util.talk.mainStationPath(window.urb.ship)]
    };
    return Dispatcher.handleViewAction({
      type: 'newItem',
      index: index,
      item: item
    });
  },
  setItem: function(item, key, val) {
    var set;
    item.version += 1;
    if (item.version === 1) {
      item[key] = val;
      item.created = Number(item.created);
      item.date_modified = Number(item.date_modified);
      item.date_created = Number(item.date_created);
      return Persistence.put({
        "new": item
      });
    } else {
      set = {};
      key = key.split('_').join('-');
      set[key] = val;
      return Persistence.put({
        old: {
          id: item.id,
          version: item.version,
          dif: {
            set: set
          }
        }
      });
    }
  },
  ownItem: function(arg, own) {
    var id, o, version;
    id = arg.id, version = arg.version;
    o = {};
    o[own] = null;
    version += 1;
    return Persistence.put({
      old: {
        id: id,
        version: version,
        dif: {
          own: o
        }
      }
    });
  },
  removeItem: function(arg) {
    var id;
    id = arg.id;
    Persistence.put({
      audience: {
        id: id,
        to: []
      }
    });
    return Dispatcher.handleViewAction({
      type: 'archiveItem',
      id: id
    });
  },
  setAudience: function(arg, to) {
    var id;
    id = arg.id;
    Persistence.put({
      audience: {
        id: id,
        to: to
      }
    });
    return Dispatcher.handleViewAction({
      type: 'setAudienece',
      id: id,
      to: to
    });
  },
  addComment: function(arg, val) {
    var id, version;
    id = arg.id, version = arg.version;
    version += 1;
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
  moveItem: function(list, to, from) {
    var sort;
    sort = _.clone(list);
    sort.splice(to, 0, sort.splice(from, 1)[0]);
    Persistence.put({
      sort: sort
    });
    return Dispatcher.handleViewAction({
      list: sort,
      to: to,
      from: from,
      type: 'moveItems'
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

ref = React.DOM, div = ref.div, h1 = ref.h1, label = ref.label;

module.exports = recl({
  onClick: function(e) {
    var b;
    switch (this.props.filters['done']) {
      case null:
        b = true;
        break;
      case true:
        b = false;
        break;
      case false:
        b = null;
    }
    return this.props.onChange('done', b);
  },
  onKeyDown: function(e) {
    if (e.keyCode === 13) {
      e.stopPropagation();
      e.preventDefault();
      return this.change(e);
    }
  },
  onBlur: function(e) {
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
      filter: 'done',
      key: 'done',
      title: ''
    }, {
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
        var filter, input, key, title;
        filter = arg.filter, key = arg.key, title = arg.title;
        input = div({
          contentEditable: true,
          className: 'input ib',
          onKeyDown: _this.onKeyDown,
          onBlur: _this.onBlur
        }, _this.props.filters[filter]);
        if (filter === 'done') {
          input = div({
            className: 'input-bool ib ' + _this.props.filters[key],
            onClick: _this.onClick
          }, "");
        }
        return div({
          key: key,
          'data-key': key,
          className: filter + " filter ib"
        }, [label({}, title), input]);
      };
    })(this)));
  }
});


},{}],3:[function(require,module,exports){
var Field, WorkActions, div, rece, recl, ref, textarea,
  slice = [].slice;

recl = React.createClass;

rece = React.createElement;

ref = React.DOM, div = ref.div, textarea = ref.textarea;

WorkActions = require('../actions/WorkActions.coffee');

Field = recl({
  displayName: 'Field',
  getInitialState: function() {
    return {
      invalid: false
    };
  },
  shouldComponentUpdate: function(props) {
    var ref1;
    while ((ref1 = this.oldValue) != null ? ref1.length : void 0) {
      if (this.oldValue[0] === props.defaultValue) {
        return false;
      } else {
        this.oldValue.shift();
      }
    }
    return true;
  },
  render: function() {
    var className, elem, props, ref1, ref2;
    className = ((ref1 = this.props.className) != null ? ref1 : this.props._key) + " field ib";
    if (this.state.invalid) {
      className += " invalid";
    }
    elem = (ref2 = this.props.elem) != null ? ref2 : "div";
    props = _.extend({}, this.props, {
      onKeyUp: this.onKeyUp,
      ref: 'input',
      defaultValue: this.props.render(this.props.defaultValue),
      className: 'input ib'
    });
    return div({
      className: className
    }, elem === 'textarea' ? textarea(props) : (props.contentEditable = true, rece(elem, props, props.defaultValue)));
  },
  onKeyUp: function(e) {
    var $t, val;
    $t = $(e.target).closest('.field');
    val = this.parse(this.getVal());
    if (!this.validate(val)) {
      this.setState({
        invalid: true
      });
      return;
    }
    this.setState({
      invalid: false
    });
    if (!this.equal(this.props.defaultValue, val)) {
      if (this.oldValue == null) {
        this.oldValue = [];
      }
      this.oldValue.push(val);
      if (this.to) {
        clearTimeout(this.to);
      }
      return this.to = setTimeout((function(_this) {
        return function() {
          return WorkActions.setItem(_this.props.item, _this.props._key, val);
        };
      })(this), 1000);
    }
  },
  getVal: function() {
    if (this.props.elem === 'textarea') {
      return $(this.refs.input.getDOMNode()).val();
    } else {
      return $(this.refs.input.getDOMNode()).text();
    }
  },
  parse: function(text) {
    var d;
    switch (this.props._key) {
      case 'tags':
        return text.trim().split(" ");
      case 'audience':
        return text.trim().split(" ").map(function(a) {
          return "~" + a;
        });
      case 'date_due':
        d = text.slice(1).replace(/\./g, "-");
        if (d.length < 8) {
          return NaN;
        }
        return new Date(d).valueOf();
      default:
        return text;
    }
  },
  equal: function(vol, val) {
    switch (this.props._key) {
      case 'tags':
      case 'audience':
        return _.xor(vol, val).length === 0;
      case 'date_due':
        return vol.valueOf() === val;
      default:
        return vol === val;
    }
  },
  validate: function(val) {
    var a, i, len, ref1, rest, ship, station;
    switch (this.props._key) {
      case 'date_due':
        return !isNaN(val);
      case 'audience':
        for (i = 0, len = val.length; i < len; i++) {
          a = val[i];
          ref1 = a.split("/"), ship = ref1[0], station = ref1[1], rest = 3 <= ref1.length ? slice.call(ref1, 2) : [];
          if (!((rest.length === 0) && ship && station)) {
            return false;
          }
          if (ship[0] !== "~") {
            return false;
          }
          if (ship < 3) {
            return false;
          }
          if (station < 3) {
            return false;
          }
        }
        return true;
      default:
        return true;
    }
  }
});

module.exports = recl({
  displayName: 'Item',
  onDragStart: function(e) {
    var $t;
    if (!this.props.draggable) {
      e.preventDefault();
      return;
    }
    $t = $(e.target);
    this.dragged = $t.closest('.item');
    e.dataTransfer.effectAllowed = 'move';
    e.dataTransfer.setData('text/html', e.currentTarget);
    return this.props._dragStart(e, this);
  },
  onDragEnd: function(e) {
    return this.props._dragEnd(e, this);
  },
  onKeyDown: function(e) {
    var kc;
    this.props.title_keyDown(e, this);
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
  onFocus: function(e) {
    return this.props._focus(e, this);
  },
  _markDone: function(e) {
    return WorkActions.setItem(this.props.item, 'done', this.props.item.done == null);
  },
  _changeStatus: function(e) {
    var own;
    if (this.props.item.status === 'released') {
      return;
    }
    if (this.props.item.status === 'accepted' && this.formatOwner(this.props.item.owner) !== window.urb.ship) {
      return;
    }
    if (this.props.item.status === "announced") {
      own = "claim";
    }
    if (this.props.item.status === "accepted") {
      own = "announce";
    }
    return WorkActions.ownItem(this.props.item, own);
  },
  _submitComment: function(e) {
    var $input, val;
    $input = $(e.target).closest('.item').find('.comment .input');
    val = $input.text();
    if (val.length === 0) {
      return;
    }
    WorkActions.addComment(this.props.item, val);
    return $input.text('');
  },
  formatDate: function(d, l) {
    var _d;
    if (d === null) {
      return "";
    }
    _d = "~" + (d.getFullYear()) + "." + (d.getMonth() + 1) + "." + (d.getDate());
    if (l) {
      _d += ".." + (d.getHours()) + "." + (d.getMinutes()) + "." + (d.getSeconds());
    }
    return _d;
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
  renderField: function(_key, props, render) {
    var defaultValue;
    if (render == null) {
      render = _.identity;
    }
    defaultValue = this.props.item[_key];
    return rece(Field, $.extend(props, {
      render: render,
      _key: _key,
      item: this.props.item,
      defaultValue: defaultValue
    }));
  },
  renderTopField: function(key, props, format) {
    var _props, ref1;
    _props = _.extend({
      className: ((ref1 = props.className) != null ? ref1 : key) + " top"
    }, props);
    return this.renderField(key, _props, format);
  },
  componentDidMount: function() {
    var formatDate;
    formatDate = this.formatDate;
    return setInterval(function() {
      return $('.new.comment .date').text(formatDate(new Date(), true));
    }, 1000);
  },
  render: function() {
    var action, discussion, itemClass;
    itemClass = 'item';
    if (this.state.expand) {
      itemClass += ' expand';
    }
    discussion = _.clone(this.props.item.discussion);
    discussion.reverse();
    action = "";
    if (this.props.item.status === 'announced') {
      action = "claim";
    }
    if (this.props.item.status === 'accepted' && this.formatOwner(this.props.item.owner) === window.urb.ship) {
      action = "release";
    }
    return div({
      className: itemClass,
      draggable: true,
      onDragStart: this.onDragStart,
      onDragEnd: this.onDragEnd
    }, [
      div({
        className: 'header'
      }, [
        div({
          className: 'owner ib'
        }, this.formatOwner(this.props.item.owner)), div({
          className: 'status ib action-' + (action.length > 0),
          'data-key': 'status',
          onClick: this._changeStatus
        }, [
          div({
            className: 'label'
          }, this.props.item.status), div({
            className: 'action a'
          }, action)
        ]), this.renderField('audience', {}, this.formatAudience)
      ]), div({
        className: 'sort ib top'
      }, this.props.item.sort), div({
        className: 'done ib done-' + (this.props.item.done != null),
        onClick: this._markDone
      }, ''), this.renderTopField('title', {
        onFocus: this.onFocus,
        onKeyDown: this.onKeyDown
      }), this.renderTopField('date_due', {
        className: 'date'
      }, this.formatDate), this.renderTopField('tags', {}, function(tags) {
        return tags.join(" ");
      }), div({
        className: 'expand ib',
        onClick: (function(_this) {
          return function(e) {
            return _this.setState({
              expand: !_this.state.expand
            });
          };
        })(this)
      }, div({
        className: 'caret left'
      }, "")), this.renderField('description', {
        elem: "textarea"
      }), div({
        className: "hr"
      }, ""), div({
        className: "discussion"
      }, [
        div({
          className: "comments"
        }, discussion.map((function(_this) {
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
              }, _this.formatDate(slug.date, true)), div({
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
          }, this.formatDate(new Date(), true)), div({
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

ref = React.DOM, div = ref.div, h1 = ref.h1, input = ref.input, textarea = ref.textarea;

WorkStore = require('../stores/WorkStore.coffee');

WorkActions = require('../actions/WorkActions.coffee');

ItemComponent = require('./ItemComponent.coffee');

ListeningComponent = require('./ListeningComponent.coffee');

FilterComponent = require('./FilterComponent.coffee');

SortComponent = require('./SortComponent.coffee');

module.exports = recl({
  stateFromStore: function() {
    window.canSort = WorkStore.canSort();
    return {
      list: WorkStore.getList(),
      canSort: WorkStore.canSort(),
      listening: WorkStore.getListening(),
      sorts: WorkStore.getSorts(),
      filters: WorkStore.getFilters(),
      expand: false,
      updated: WorkStore.getUpdated()
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
      selected: i.props.index
    });
  },
  _dragStart: function(e, i) {
    return this.dragged = i.dragged;
  },
  _dragEnd: function(e, i) {
    var from, id, to;
    from = Number(this.dragged.closest('.item-wrap').attr('data-index'));
    to = Number(this.over.closest('.item-wrap').attr('data-index'));
    if (from < to) {
      to--;
    }
    if (this.drop === 'after') {
      to++;
    }
    WorkActions.moveItem((function() {
      var j, len, ref1, results;
      ref1 = this.state.list;
      results = [];
      for (j = 0, len = ref1.length; j < len; j++) {
        id = ref1[j].id;
        results.push(id);
      }
      return results;
    }).call(this), to, from);
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
  title_keyDown: function(e, i) {
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
          WorkActions.removeItem(i.props.item);
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
  componentDidUpdate: function(_props, _state) {
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
      }, _.map(this.state.list, (function(_this) {
        return function(item, index) {
          return div({
            className: 'item-wrap',
            key: item.id,
            'data-index': index
          }, rece(ItemComponent, {
            item: item,
            index: index,
            _focus: _this._focus,
            title_keyDown: _this.title_keyDown,
            draggable: _this.state.canSort,
            _dragStart: _this._dragStart,
            _dragEnd: _this._dragEnd
          }));
        };
      })(this)))
    ]);
  }
});


},{"../actions/WorkActions.coffee":1,"../stores/WorkStore.coffee":15,"./FilterComponent.coffee":2,"./ItemComponent.coffee":3,"./ListeningComponent.coffee":5,"./SortComponent.coffee":6}],5:[function(require,module,exports){
var div, h1, input, rece, recl, ref, textarea;

recl = React.createClass;

rece = React.createElement;

ref = React.DOM, div = ref.div, h1 = ref.h1, input = ref.input, textarea = ref.textarea;

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

ref = React.DOM, div = ref.div, h1 = ref.h1, button = ref.button, label = ref.label;

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

ref = React.DOM, div = ref.div, h1 = ref.h1;

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
var Dispatcher, EventEmitter, WorkStore, _filters, _list, _listening, _sorts, _tasks, _updated, assign;

EventEmitter = require('events').EventEmitter;

assign = require('object-assign');

Dispatcher = require('../dispatcher/Dispatcher.coffee');

_tasks = {};

_list = [];

_listening = [];

_updated = Date.now();

_filters = {
  done: null,
  owner: null,
  tags: null,
  audience: null,
  status: null
};

_sorts = {
  sort: 0,
  title: 0,
  owner: 0,
  date_due: 0
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
    var sort, tasks;
    sort = arg.sort, tasks = arg.tasks;
    sort.map((function(_this) {
      return function(id, index) {
        if (!_tasks[id]) {
          _list.splice(index, 0, id);
        }
        if (tasks[id]) {
          return _tasks[id] = _this.itemFromData(tasks[id], index);
        }
      };
    })(this));
    return _updated = Date.now();
  },
  getUpdated: function() {
    return _updated;
  },
  getList: function(key) {
    var _k, _v, add, c, i, id, k, len, list, task, v;
    list = [];
    for (i = 0, len = _list.length; i < len; i++) {
      id = _list[i];
      task = _tasks[id];
      if (task.archived) {
        continue;
      }
      add = true;
      for (_k in _filters) {
        _v = _filters[_k];
        if (_v === null) {
          continue;
        }
        c = task[_k];
        add = (function() {
          switch (_k) {
            case 'tags':
            case 'audience':
              return _.intersection(c, _v).length !== 0;
            case 'owner':
              return c === _v.replace(/\~/g, "");
            case 'done':
              return !!c === _v;
            default:
              return c === _v;
          }
        })();
        if (!add) {
          break;
        }
      }
      if (add) {
        list.push(task);
      }
    }
    if (_.uniq(_.values(_sorts)).length > 1) {
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
  newItem: function(arg) {
    var index, item;
    index = arg.index, item = arg.item;
    _list.splice(index, 0, item.id);
    return _tasks[item.id] = item;
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
  canSort: function() {
    var k, v;
    for (k in _sorts) {
      v = _sorts[k];
      if (k === "sort" && v === 1) {
        return true;
      } else if (v !== 0) {
        return false;
      }
    }
    return true;
  },
  itemFromData: function(item, index) {
    var _item;
    if (index == null) {
      index = 0;
    }
    _item = _.extend({
      sort: index
    }, item);
    _item.date_modified = new Date(item.date_modified);
    _item.date_created = new Date(item.date_created);
    if (item.date_due != null) {
      _item.date_due = new Date(item.date_due);
    }
    if (item.done != null) {
      _item.done = new Date(item.done);
    }
    _item.discussion = item.discussion.map(function(arg) {
      var body, date, ship;
      ship = arg.ship, body = arg.body, date = arg.date;
      return {
        ship: ship,
        body: body,
        date: new Date(date)
      };
    });
    return _item;
  },
  moveItems: function(arg) {
    var from, list, to;
    list = arg.list, to = arg.to, from = arg.from;
    _tasks[_list[from]].sort = _tasks[_list[to]].sort;
    return _list = list;
  },
  setAudience: function(arg) {
    var id, to;
    id = arg.id, to = arg.to;
    return _tasks[id].audience = to;
  },
  archiveItem: function(arg) {
    var id;
    id = arg.id;
    return _tasks[id].archived = true;
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
