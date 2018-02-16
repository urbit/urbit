(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Dispatcher, Persistence, _persistence, util;

util = require('../util.coffee');

Dispatcher = require('../dispatcher/Dispatcher.coffee');

_persistence = require('../persistence/MessagePersistence.coffee');

Persistence = _persistence({
  MessageActions: module.exports = {
    setFilter: function(station) {
      return Dispatcher.handleViewAction({
        station: station,
        type: "messages-filter"
      });
    },
    clearFilter: function() {
      return Dispatcher.handleViewAction({
        type: "messages-filter-clear"
      });
    },
    loadMessages: function(messages, last, get) {
      return Dispatcher.handleServerAction({
        messages: messages,
        last: last,
        get: get,
        type: "messages-load"
      });
    },
    listenStation: function(station, date) {
      var now;
      if (!date) {
        date = (now = new Date(), now.setSeconds(0), now.setMilliseconds(0), new Date(now - 6 * 3600 * 1000));
      }
      Dispatcher.handleViewAction({
        type: "messages-fetch"
      });
      return Persistence.listenStation(station, date);
    },
    listeningStation: function(station) {
      return Dispatcher.handleViewAction({
        station: station,
        type: "messages-listen"
      });
    },
    setTyping: function(state) {
      return Dispatcher.handleViewAction({
        state: state,
        type: "messages-typing"
      });
    },
    getMore: function(station, start, end) {
      Dispatcher.handleViewAction({
        type: "messages-fetch"
      });
      return Persistence.get(station, start, end);
    },
    sendMessage: function(txt, audience, global) {
      var message, speech;
      if (global == null) {
        global = urb.user === urb.ship;
      }
      audience = _.uniq(audience);
      speech = {
        lin: {
          msg: txt,
          pat: false
        }
      };
      if (txt[0] === "@") {
        speech.lin.msg = speech.lin.msg.slice(1).trim();
        speech.lin.pat = true;
      } else if (txt[0] === "#") {
        speech = {
          exp: {
            exp: speech.lin.msg.slice(1).trim()
          }
        };
      } else if (window.urb.util.isURL(txt)) {
        speech = {
          url: txt
        };
      }
      message = {
        aut: window.urb.user,
        uid: util.uuid32(),
        aud: audience,
        wen: Date.now(),
        sep: speech
      };
      Dispatcher.handleViewAction({
        message: message,
        type: "message-send"
      });
      return Persistence.sendMessage(message);
    }
  }
});


},{"../dispatcher/Dispatcher.coffee":9,"../persistence/MessagePersistence.coffee":11,"../util.coffee":15}],2:[function(require,module,exports){
var Dispatcher, Persistence, _persistence, serverAction, viewAction;

Dispatcher = require('../dispatcher/Dispatcher.coffee');

serverAction = function(f) {
  return function() {
    return Dispatcher.handleServerAction(f.apply(this, arguments));
  };
};

viewAction = function(f) {
  return function() {
    return Dispatcher.handleViewAction(f.apply(this, arguments));
  };
};

_persistence = require('../persistence/StationPersistence.coffee');

Persistence = _persistence({
  StationActions: module.exports = {
    loadGlyphs: serverAction(function(glyphs) {
      return {
        glyphs: glyphs,
        type: "glyphs-load"
      };
    }),
    loadMembers: serverAction(function(members) {
      return {
        members: members,
        type: "members-load"
      };
    }),
    loadStations: serverAction(function(stations) {
      return {
        stations: stations,
        type: "stations-load"
      };
    }),
    loadConfig: serverAction(function(station, config) {
      return {
        station: station,
        config: config,
        type: "config-load"
      };
    }),
    setTyping: viewAction(function(station, state) {
      return {
        station: station,
        state: state,
        type: "typing-set"
      };
    }),
    setAudience: viewAction(function(audience) {
      return {
        audience: audience,
        type: "station-set-audience"
      };
    }),
    setValidAudience: viewAction(function(valid) {
      return {
        valid: valid,
        type: "station-set-valid-audience"
      };
    }),
    toggleAudience: viewAction(function(station) {
      return {
        station: station,
        type: "station-audience-toggle"
      };
    }),
    switchStation: viewAction(function(station) {
      return {
        station: station,
        type: "station-switch"
      };
    }),
    listeningStation: viewAction(function(station) {
      return {
        station: station,
        type: "station-listen"
      };
    }),
    createStation: function(station) {
      addStation(station);
      return Persistence.createStation(station);
    },
    addStation: function(station) {
      return Dispatcher.handleViewAction({
        station: station,
        type: "station-create"
      });
    },
    remStation: function(station) {
      return Dispatcher.handleViewAction({
        station: station,
        type: "station-remove"
      });
    },
    listen: function() {
      return Persistence.listen();
    },
    ping: function(_ping) {
      return Persistence.ping(_ping);
    },
    removeStation: function(station) {
      return Persistence.removeStation(station);
    },
    listenStation: function(station) {
      return Persistence.listenStation(station, {
        'group': 'group',
        'glyph': 'glyph',
        'cabal': 'cabal'
      });
    },
    createStation: function(name) {
      return Persistence.createStation(name);
    },
    addSources: function(station, sources) {
      return Persistence.addSources(station, sources);
    },
    remSources: function(station, sources) {
      return Persistence.remSources(station, sources);
    }
  }
});


},{"../dispatcher/Dispatcher.coffee":9,"../persistence/StationPersistence.coffee":12}],3:[function(require,module,exports){
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


},{}],4:[function(require,module,exports){
var Ship, div, input, recl, ref, rele, textarea;

Ship = window.tree.util.components.ship;

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, input = ref.input, textarea = ref.textarea;

module.exports = recl({
  displayName: "Member",
  render: function() {
    var k, ref1, ship;
    ship = (ref1 = this.props.ship) != null ? ref1 : "";
    k = "ship";
    if (this.props.presence) {
      k += " " + this.props.presence;
    }
    return div({
      className: "iden",
      key: "iden"
    }, [
      rele(Ship, {
        ship: ship
      })
    ]);
  }
});


},{}],5:[function(require,module,exports){
var Member, a, clas, div, h2, h3, label, pre, recl, ref, util,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

util = require('../util.coffee');

clas = require('classnames');

recl = React.createClass;

ref = React.DOM, div = ref.div, pre = ref.pre, a = ref.a, label = ref.label, h2 = ref.h2, h3 = ref.h3;

Member = require('./MemberComponent.coffee');

module.exports = recl({
  displayName: "Message",
  lz: function(n) {
    if (n < 10) {
      return "0" + n;
    } else {
      return "" + n;
    }
  },
  convTime: function(time) {
    var d, h, m, s;
    d = new Date(time);
    h = this.lz(d.getHours());
    m = this.lz(d.getMinutes());
    s = this.lz(d.getSeconds());
    return "~" + h + "." + m + "." + s;
  },
  _handleAudi: function(e) {
    var audi;
    audi = _.map($(e.target).closest('.path').find('div'), function(div) {
      return $(div).text();
    });
    return this.props._handleAudi(audi);
  },
  _handlePm: function(e) {
    var user;
    if (!this.props._handlePm) {
      return;
    }
    user = $(e.target).closest('.iden').text();
    if (user[0] === "~") {
      user = user.slice(1);
    }
    if (user.toLowerCase() === 'system') {
      return;
    }
    return this.props._handlePm(user);
  },
  renderSpeech: function(arg) {
    var app, exp, fat, inv, ire, lin, prex, ref1, url, x;
    lin = arg.lin, url = arg.url, exp = arg.exp, ire = arg.ire, fat = arg.fat, inv = arg.inv, app = arg.app;
    switch (false) {
      case !lin:
        return lin.msg;
      case !url:
        return a({
          href: url,
          target: "_blank",
          rel: "noopener",
          key: "speech"
        }, url);
      case !exp:
        exp.res = exp.res || ["evaluating..."];
        return div({}, exp.exp, div({
          className: "fat"
        }, pre({}, exp.res.join("\n"))));
      case !ire:
        return this.renderSpeech(ire.sep);
      case !fat:
        return div({}, this.renderSpeech(fat.sep), div({
          className: "fat"
        }, this.renderAttache(fat.tac)));
      case !inv:
        prex = (ref1 = inv.inv) != null ? ref1 : {
          "invited you to ": "banished you from "
        };
        return prex + inv.cir;
      case !app:
        return this.renderSpeech(app.sep);
      default:
        return "Unknown speech type:" + ((function() {
          var results;
          results = [];
          for (x in arguments[0]) {
            results.push(" %" + x);
          }
          return results;
        }).apply(this, arguments)).join('');
    }
  },
  renderAttache: function(arg) {
    var name, tank, text;
    text = arg.text, tank = arg.tank, name = arg.name;
    switch (false) {
      case text == null:
        return pre({}, text);
      case tank == null:
        return pre({}, tank.join("\n"));
      case name == null:
        return pre({}, name.nom, ":\n", this.renderAttache(name.tac));
    }
  },
  classesInSpeech: function(arg) {
    var app, exp, fat, inv, ire, lin, url;
    lin = arg.lin, url = arg.url, exp = arg.exp, ire = arg.ire, fat = arg.fat, inv = arg.inv, app = arg.app;
    switch (false) {
      case !lin:
        return {
          say: lin.pat
        };
      case !url:
        return "url";
      case !exp:
        return "exp";
      case !ire:
        return this.classesInSpeech(ire.sep);
      case !fat:
        return this.classesInSpeech(fat.sep);
      case !inv:
        return {
          say: true
        };
      case !app:
        return "exp";
    }
  },
  render: function() {
    var audi, className, gam, heard, mainStation, name, speech, style, type;
    gam = this.props;
    heard = gam.heard;
    speech = gam.sep;
    if (speech == null) {
      return;
    }
    name = this.props.name ? this.props.name : "";
    audi = util.clipAudi(gam.aud).map(function(_audi) {
      return div({
        key: _audi
      }, _audi);
    });
    mainStation = util.mainStationPath(window.urb.user);
    type = indexOf.call(gam.aud, mainStation) >= 0 ? 'private' : 'public';

    /*
    if(_.filter(bouquet, ["comment"]).length > 0)
      comment = true
      for k,v of speech.mor
        if v.fat
          url = v.fat.taf.url.txt
          txt = v.fat.tor.text
        if v.app then path = v.app.txt.replace "comment on ", ""
      audi = (a {href:url}, path)
      speech = {com:{txt,url}}
     */
    className = clas('gram', (this.props.sameAs ? "same" : "first"), (heard ? "received" : "pending"), {
      'new': this.props.unseen
    }, {
      comment: false
    }, this.classesInSpeech(speech));
    style = {
      height: this.props.height,
      marginTop: this.props.marginTop
    };
    return div({
      className: className,
      'data-index': this.props.index,
      key: "message",
      style: style
    }, div({
      className: "meta",
      key: "meta"
    }, label({
      className: "type " + type,
      "data-glyph": this.props.glyph || "*"
    }), h2({
      className: 'author planet',
      onClick: this._handlePm,
      key: "member"
    }, React.createElement(Member, {
      ship: this.props.ship,
      glyph: this.props.glyph,
      key: "member"
    })), h3({
      className: "path",
      onClick: this._handleAudi,
      key: "audi"
    }, audi), h3({
      className: "time",
      key: "time"
    }, this.convTime(gam.wen))), div({
      className: "speech",
      key: "speech"
    }, this.renderSpeech(speech)));
  }
});


},{"../util.coffee":15,"./MemberComponent.coffee":4,"classnames":16}],6:[function(require,module,exports){
var FONT_SIZE, INFINITE, Infinite, Load, MESSAGE_HEIGHT_FIRST, MESSAGE_HEIGHT_FIRST_MARGIN_TOP, MESSAGE_HEIGHT_SAME, Message, MessageActions, MessageStore, StationActions, StationStore, div, recl, rele, util;

util = require('../util.coffee');

Infinite = null;

recl = React.createClass;

rele = React.createElement;

div = React.DOM.div;

MessageActions = require('../actions/MessageActions.coffee');

MessageStore = require('../stores/MessageStore.coffee');

StationActions = require('../actions/StationActions.coffee');

StationStore = require('../stores/StationStore.coffee');

Message = require('./MessageComponent.coffee');

Load = require('./LoadComponent.coffee');

INFINITE = true;

MESSAGE_HEIGHT_SAME = 27;

MESSAGE_HEIGHT_FIRST = 56 - MESSAGE_HEIGHT_SAME;

MESSAGE_HEIGHT_FIRST_MARGIN_TOP = 16;

FONT_SIZE = parseInt($('body').css('font-size').match(/(\d*)px/)[1]);

module.exports = recl({
  displayName: "Messages",
  pageSize: 200,
  paddingTop: 100,
  paddingBottom: 100,
  stateFromStore: function() {
    return {
      messages: MessageStore.getAll(),
      oldest: MessageStore.getOldest(),
      fetching: MessageStore.getFetching(),
      listening: MessageStore.getListening(),
      station: StationStore.getStation(),
      stations: StationStore.getStations(),
      configs: StationStore.getConfigs(),
      typing: MessageStore.getTyping(),
      glyph: StationStore.getGlyphMap()
    };
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  _blur: function() {
    this.focused = false;
    return this.lastSeen = this.last;
  },
  _focus: function() {
    this.focused = true;
    this.lastSeen = null;
    $('.message.new').removeClass('new');
    return document.title = document.title.replace(/\ \([0-9]*\)/, "");
  },
  atScrollEdge: function() {
    switch (this.props.chrono) {
      case "reverse":
        return $(window).height() < $(window).scrollTop() + $(window)[0].innerHeight + this.paddingBottom;
      default:
        return $(window).scrollTop() < this.paddingTop;
    }
  },
  checkMore: function() {
    var end;
    this.state.oldest = MessageStore.getOldest();
    if (this.atScrollEdge() && this.state.fetching === false && this.state.oldest && this.state.oldest > 0) {
      end = this.state.oldest - this.pageSize;
      if (end < 0) {
        end = 0;
      }
      this.lastLength = this.length;
      if (end >= 0) {
        return MessageActions.getMore(this.state.station, this.state.oldest + 1, end);
      }
    }
  },
  setAudience: function() {
    var laudi;
    if (this.state.typing || !this.last) {
      return;
    }
    laudi = this.last.aud;
    if ((_.isEmpty(laudi)) || !_(laudi).difference(this.state.audi).isEmpty()) {
      return StationActions.setAudience(this.last.aud);
    }
  },
  sortedMessages: function(messages) {
    var station;
    station = this.state.station;
    return _.sortBy(messages, (function(_this) {
      return function(message) {
        return message.key;
      };
    })(this));
  },
  componentWillMount: function() {
    return Infinite = window.Infinite;
  },
  componentDidMount: function() {
    MessageStore.addChangeListener(this._onChangeStore);
    StationStore.addChangeListener(this._onChangeStore);
    if (this.state.station && this.state.listening.indexOf(this.state.station) === -1) {
      MessageActions.listenStation(this.state.station);
    }
    if (this.props["static"] == null) {
      $(window).on('scroll', this.checkMore);
    }
    if (this.props.chrono !== "reverse") {
      util.scrollToBottom();
    }
    this.focused = true;
    $(window).on('blur', this._blur);
    $(window).on('focus', this._focus);
    return $(window).on('resize', _.debounce((function(_this) {
      return function() {
        return _this.forceUpdate();
      };
    })(this), 250));
  },
  componentWillUpdate: function(props, state) {
    return this.scrollBottom = $(document).height() - ($(window).scrollTop() + window.innerHeight);
  },
  componentDidUpdate: function(_props, _state) {
    var _messages, _oldMessages, appendedToBottom, d, ref, ref1, ref2, setOffset, t;
    _messages = this.sortedMessages(this.state.messages);
    _oldMessages = this.sortedMessages(_state.messages);
    appendedToBottom = (((ref = _.last(_messages)) != null ? ref.key : void 0) == null) || ((ref1 = _.last(_messages)) != null ? ref1.key : void 0) > ((ref2 = _.last(_oldMessages)) != null ? ref2.key : void 0);
    setOffset = $(document).height() - window.innerHeight - this.scrollBottom;
    if (this.props.chrono !== "reverse") {
      if (!(this.scrollBottom > 0 && appendedToBottom)) {
        $(window).scrollTop(setOffset);
      }
    }
    if (this.focused === false && this.last !== this.lastSeen) {
      d = _messages.length - _messages.indexOf(this.lastSeen) - 1;
      t = document.title;
      if (document.title.match(/\([0-9]*\)/)) {
        return document.title = document.title.replace(/\([0-9]*\)/, "(" + d + ")");
      } else {
        return document.title = document.title + (" (" + d + ")");
      }
    }
  },
  componentWillUnmount: function() {
    MessageStore.removeChangeListener(this._onChangeStore);
    return StationStore.removeChangeListener(this._onChangeStore);
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  _handlePm: function(user) {
    var audi;
    if (this.props['audience-lock'] != null) {
      return;
    }
    audi = [util.mainStationPath(user)];
    if (user === window.urb.user) {
      audi.pop();
    }
    return StationActions.setAudience(audi);
  },
  _handleAudi: function(audi) {
    return StationActions.setAudience(audi);
  },
  _getSpeechArr: function(spec) {
    if (spec.lin != null) {
      return spec.lin.msg.split(/(\s|-)/);
    } else if (spec.url != null) {
      return spec.url.split(/(\s|-)/);
    } else if (spec.exp != null) {
      return [spec.exp.exp];
    } else if (spec.app != null) {
      return this._getSpeechArr(spec.app.sep);
    } else if (spec.fat != null) {
      return this._getSpeechArr(spec.fat.sep);
    } else {
      return [];
    }
  },
  render: function() {
    var _messageGroups, _messages, aud, body, canvas, context, fetching, height, i, index, lastIndex, lastSaid, len, lineNums, marginTop, message, messageHeights, messages, mez, nowSaid, ref, sameAs, speechArr, speechLength, station;
    station = this.state.station;
    messages = this.sortedMessages(this.state.messages);
    this.last = messages[messages.length - 1];
    if (((ref = this.last) != null ? ref.aut : void 0) && this.last.aut === window.urb.user) {
      this.lastSeen = this.last;
    }
    this.length = messages.length;
    setTimeout(((function(_this) {
      return function() {
        if (_this.length < _this.pageSize) {
          return _this.checkMore();
        }
      };
    })(this)), 1);
    lastIndex = this.lastSeen ? messages.indexOf(this.lastSeen) + 1 : null;
    lastSaid = null;
    messageHeights = [];
    canvas = document.createElement('canvas');
    context = canvas.getContext('2d');
    speechLength = $('.grams').width() - (FONT_SIZE * 1.875);
    _messageGroups = [[]];
    for (index = i = 0, len = messages.length; i < len; index = ++i) {
      message = messages[index];
      if (message.sep.app) {
        message.aut = message.sep.app.app;
      }
      nowSaid = [message.aut, message.aud];
      sameAs = _.isEqual(lastSaid, nowSaid);
      lastSaid = nowSaid;
      lineNums = 1;
      speechArr = this._getSpeechArr(message.sep);
      context.font = FONT_SIZE + 'px bau';
      _.reduce(_.tail(speechArr), function(base, word) {
        if (context.measureText(base + word).width > speechLength) {
          lineNums += 1;
          if (word === ' ') {
            return '';
          } else if (word === '-') {
            return _.head(base.split(/\s|-/).reverse()) + word;
          } else {
            return word;
          }
        } else {
          return base + word;
        }
      }, _.head(speechArr));
      if (INFINITE) {
        height = MESSAGE_HEIGHT_SAME * lineNums;
        if (sameAs) {
          marginTop = 0;
        } else {
          height += MESSAGE_HEIGHT_FIRST;
          marginTop = MESSAGE_HEIGHT_FIRST_MARGIN_TOP;
        }
      } else {
        height = null;
        marginTop = null;
      }
      aud = message.aud.join(" ");
      mez = rele(Message, _.extend({}, message, {
        station: station,
        sameAs: sameAs,
        _handlePm: this._handlePm,
        _handleAudi: this._handleAudi,
        height: height,
        marginTop: marginTop,
        index: message.key,
        key: "message-" + message.key,
        ship: message.aut,
        glyph: this.state.glyph[aud] || this.props['default-glyph'],
        unseen: lastIndex && lastIndex === index
      }));
      mez.computedHeight = height + marginTop;
      if (sameAs) {
        _messageGroups[0].push(mez);
      } else {
        _messageGroups.unshift([mez]);
      }
    }
    if (this.props.chrono !== "reverse") {
      _messageGroups = _messageGroups.reverse();
    }
    _messages = _.flatten(_messageGroups);
    if ((this.props.readOnly == null) && INFINITE) {
      body = rele(Infinite, {
        useWindowAsScrollContainer: true,
        containerHeight: window.innerHeight,
        elementHeight: _.map(_messages, 'computedHeight'),
        key: "messages-infinite"
      }, _messages);
    } else {
      body = _messages;
    }
    fetching = this.state.fetching ? rele(Load, {}) : void 0;
    return div({
      className: "grams",
      key: "messages"
    }, body, fetching);
  }
});


},{"../actions/MessageActions.coffee":1,"../actions/StationActions.coffee":2,"../stores/MessageStore.coffee":13,"../stores/StationStore.coffee":14,"../util.coffee":15,"./LoadComponent.coffee":3,"./MessageComponent.coffee":5}],7:[function(require,module,exports){
var Load, Member, MessageActions, MessageStore, StationActions, StationStore, a, clas, div, h1, h2, input, label, recl, ref, rele, span, style, util,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

util = require('../util.coffee');

clas = require('classnames');

recl = React.createClass;

rele = React.createElement;

ref = React.DOM, div = ref.div, style = ref.style, input = ref.input, h1 = ref.h1, h2 = ref.h2, label = ref.label, span = ref.span, a = ref.a;

MessageStore = require('../stores/MessageStore.coffee');

StationStore = require('../stores/StationStore.coffee');

MessageActions = require('../actions/MessageActions.coffee');

StationActions = require('../actions/StationActions.coffee');

Member = require('./MemberComponent.coffee');

Load = require('./LoadComponent.coffee');

module.exports = recl({
  displayName: "Station",
  stateFromStore: function() {
    var ref1;
    return {
      audi: StationStore.getAudience(),
      members: StationStore.getMembers(),
      station: util.mainStation(),
      filter: MessageStore.getFilter(),
      stations: StationStore.getStations(),
      configs: StationStore.getConfigs(),
      fetching: MessageStore.getFetching(),
      typing: StationStore.getTyping(),
      listening: StationStore.getListening(),
      open: (((ref1 = this.state) != null ? ref1.open : void 0) ? this.state.open : null)
    };
  },
  getInitialState: function() {
    return this.stateFromStore();
  },
  componentDidMount: function() {
    this.$el = $(ReactDOM.findDOMNode());
    MessageStore.addChangeListener(this._onChangeStore);
    StationStore.addChangeListener(this._onChangeStore);
    if (this.state.listening.indexOf(this.state.station) === -1) {
      return StationActions.listenStation(this.state.station);
    }
  },
  componentWillUnmount: function() {
    return StationStore.removeChangeListener(this._onChangeStore);
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  componentWillReceiveProps: function(nextProps) {
    if (this.props.open === true && nextProps.open === false) {
      return this.setState({
        open: null
      });
    }
  },
  validateSource: function(s) {
    var src;
    src = this.state.configs[this.state.station].src;
    return indexOf.call(src, s) < 0 && indexOf.call(s, "/") >= 0 && s[0] === "~" && s.length >= 5;
  },
  onKeyUp: function(e) {
    var $input, d, v;
    $('.menu.depth-1 .add').removeClass('valid-false');
    if (e.keyCode === 13) {
      $input = $(e.target);
      v = $input.val().toLowerCase();
      if (v[0] !== "~") {
        v = "~" + v;
      }
      if (this.validateSource(v)) {
        d = new Date(new Date() - 24 * 3600 * 1000);
        v = v + "/" + window.urb.util.toDate(d);
        StationActions.addSources(this.state.station, [v]);
        $input.val('');
        return $input.blur();
      } else {
        return $('.menu.depth-1 .add').addClass('valid-false');
      }
    }
  },
  _openStation: function(e) {
    return this.setState({
      open: $(e.target).attr('data-station')
    });
  },
  _closeStation: function() {
    return this.setState({
      open: null
    });
  },
  _filterStation: function(e) {
    var station;
    station = $(e.target).attr('data-station');
    if (this.state.filter !== station) {
      return MessageActions.setFilter(station);
    } else {
      return MessageActions.clearFilter();
    }
  },
  _remove: function(e) {
    var _station;
    e.stopPropagation();
    e.preventDefault();
    _station = $(e.target).attr("data-station");
    return StationActions.remSources(this.state.station, [_station]);
  },
  render: function() {
    var _clas, member, members, parts, source, sources, sourcesSum, station;
    parts = [];
    members = [];
    if (this.state.station && this.state.configs[this.state.station]) {
      members = (function() {
        var ref1, results;
        ref1 = this.state.members;
        results = [];
        for (station in ref1) {
          members = ref1[station];
          _clas = clas({
            open: this.state.open === station,
            closed: !(this.state.open === station),
            menu: true,
            'depth-2': true
          });
          results.push(div({
            className: _clas,
            "data-members": station,
            key: station
          }, div({
            className: "contents",
            onClick: this._closeStation
          }, div({
            className: "close"
          }, "✕"), h2({}, span({}, "Members"), label({
            className: "sum"
          }, _.keys(members).length)), (function() {
            var results1;
            results1 = [];
            for (member in members) {
              results1.push(div({
                key: member
              }, div({
                className: "name"
              }, ""), div({
                className: "planet"
              }, member)));
            }
            return results1;
          })())));
        }
        return results;
      }).call(this);
    }
    if (this.state.station && this.state.configs[this.state.station]) {
      sources = (function() {
        var i, len, ref1, results;
        ref1 = this.state.configs[this.state.station].src;
        results = [];
        for (i = 0, len = ref1.length; i < len; i++) {
          source = ref1[i];
          results.push(div({
            key: source,
            className: "room"
          }, div({
            className: (this.state.open === source ? "selected" : ""),
            onClick: this._openStation,
            "data-station": source
          }, source), div({
            className: 'options'
          }, div({
            onClick: this._filterStation,
            "data-station": source
          }, this.state.filter === source ? "Clear" : "Filter"), div({
            onClick: this._remove,
            "data-station": source
          }, "Leave"))));
        }
        return results;
      }).call(this);
      sources.push(input({
        key: "placeholder",
        className: "action add",
        placeholder: "+ Listen",
        onKeyUp: this.onKeyUp
      }));
      sourcesSum = this.state.configs[this.state.station].src.length;
    } else {
      sources = "";
      sourcesSum = 0;
    }
    _clas = clas({
      open: this.props.open === true,
      closed: this.props.open !== true,
      menu: true,
      'depth-1': true
    });
    return div({
      key: "station-container"
    }, div({
      className: _clas,
      key: 'station'
    }, div({
      className: "contents"
    }, div({
      className: "close",
      onClick: this.props.toggle
    }, "✕"), h2({}, span({}, "Stations"), label({
      className: "sum"
    }, sourcesSum)), div({}, sources))), members);
  }
});


},{"../actions/MessageActions.coffee":1,"../actions/StationActions.coffee":2,"../stores/MessageStore.coffee":13,"../stores/StationStore.coffee":14,"../util.coffee":15,"./LoadComponent.coffee":3,"./MemberComponent.coffee":4,"classnames":16}],8:[function(require,module,exports){
var Audience, Member, MessageActions, MessageStore, PO, SHIPSHAPE, StationActions, StationStore, br, div, husl, input, recl, ref, textToHTML, textarea, util,
  hasProp = {}.hasOwnProperty;

util = require('../util.coffee');

recl = React.createClass;

ref = React.DOM, div = ref.div, br = ref.br, input = ref.input, textarea = ref.textarea;

husl = require('husl');

MessageActions = require('../actions/MessageActions.coffee');

MessageStore = require('../stores/MessageStore.coffee');

StationActions = require('../actions/StationActions.coffee');

StationStore = require('../stores/StationStore.coffee');

Member = require('./MemberComponent.coffee');

SHIPSHAPE = /^~?([a-z]{3}|[a-z]{6}(-[a-z]{6}){0,3}|[a-z]{6}(-[a-z]{6}){3}(--[a-z]{6}(-[a-z]{6}){3})+)$/;

PO = 'dozmarbinwansamlitsighidfidlissogdirwacsabwissib\nrigsoldopmodfoglidhopdardorlorhodfolrintogsilmir\nholpaslacrovlivdalsatlibtabhanticpidtorbolfosdot\nlosdilforpilramtirwintadbicdifrocwidbisdasmidlop\nrilnardapmolsanlocnovsitnidtipsicropwitnatpanmin\nritpodmottamtolsavposnapnopsomfinfonbanporworsip\nronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig\nsivtagpadsaldivdactansidfabtarmonranniswolmispal\nlasdismaprabtobrollatlonnodnavfignomnibpagsopral\nbilhaddocridmocpacravripfaltodtiltinhapmicfanpat\ntaclabmogsimsonpinlomrictapfirhasbosbatpochactid\nhavsaplindibhosdabbitbarracparloddosbortochilmac\ntomdigfilfasmithobharmighinradmashalraglagfadtop\nmophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc\nnimlarfitwalrapsarnalmoslandondanladdovrivbacpol\nlaptalpitnambonrostonfodponsovnocsorlavmatmipfap\n\nzodnecbudwessevpersutletfulpensytdurwepserwylsun\nrypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex\nlunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb\npyldulhetmevruttylwydtepbesdexsefwycburderneppur\nrysrebdennutsubpetrulsynregtydsupsemwynrecmegnet\nsecmulnymtevwebsummutnyxrextebfushepbenmuswyxsym\nselrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel\nsyptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed\nlytdusnebrumtynseglyxpunresredfunrevrefmectedrus\nbexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer\ntenlusnussyltecmexpubrymtucfyllepdebbermughuttun\nbylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl\nwedducfurfexnulluclennerlexrupnedlecrydlydfenwel\nnydhusrelrudneshesfetdesretdunlernyrsebhulryllud\nremlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun\nlyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes';

textToHTML = function(txt) {
  return {
    __html: $('<div>').text(txt).html()
  };
};

Audience = recl({
  displayName: "Audience",
  onKeyDown: function(e) {
    if (e.keyCode === 13) {
      e.preventDefault();
      if (this.props.validate()) {
        setTimeout((function() {
          return $('.writing').focus();
        }), 0);
        return false;
      }
    }
    if (e.keyCode === 9) {
      e.preventDefault();
      this._autoCompleteAudience();
      return false;
    } else if ((this.tabAudList != null) && e.keyCode !== 16) {
      this.tabAudList = null;
      return this.tabAudIndex = null;
    }
  },
  _autoCompleteAudience: function() {
    var aud, g, i, j, len, len1, ref1, ref2, s, stations, txt;
    txt = $('#audience .input').text().trim();
    if (this.tabAudList == null) {
      this.tabAudList = [];
      if (txt.length === 1 && StationStore.getGlyphs()[txt[0]]) {
        ref1 = this._getGlyphExpansions(txt[0]);
        for (i = 0, len = ref1.length; i < len; i++) {
          s = ref1[i];
          this.tabAudList.push(s[0]);
        }
      } else {
        if (!(txt[0] === '~')) {
          txt = '~' + txt;
        }
        ref2 = StationStore.getGlyphs();
        for (g in ref2) {
          stations = ref2[g];
          for (j = 0, len1 = stations.length; j < len1; j++) {
            aud = stations[j];
            if (aud[0].indexOf(txt) === 0 && this.tabAudList.indexOf(aud[0]) < 0) {
              this.tabAudList.push(aud[0]);
            }
          }
        }
      }
    }
    if ((this.tabAudList != null) && this.tabAudList.length > 0) {
      if (this.tabAudIndex != null) {
        if (event.shiftKey) {
          this.tabAudIndex--;
        } else {
          this.tabAudIndex++;
        }
        this.tabAudIndex = (this.tabAudIndex % this.tabAudList.length + this.tabAudList.length) % this.tabAudList.length;
      } else {
        this.tabAudIndex = 0;
      }
      return StationActions.setAudience(this.tabAudList[this.tabAudIndex].split(/\ +/));
    }
  },
  _getGlyphExpansions: function(g) {
    var glyphs;
    glyphs = StationStore.getGlyphs();
    if (glyphs[g]) {
      return glyphs[g];
    }
  },
  render: function() {
    return div({
      className: 'audience',
      id: 'audience',
      key: 'audience'
    }, div({
      className: "input valid-" + this.props.valid,
      key: 'input',
      contentEditable: this.props.editable ? true : void 0,
      onKeyDown: this.onKeyDown,
      onBlur: this.props.onBlur,
      dangerouslySetInnerHTML: textToHTML(this.props.audi.join(" "))
    }));
  }
});

module.exports = recl({
  displayName: "Writing",
  set: function() {
    if (window.localStorage && this.$message) {
      return window.localStorage.setItem('writing', this.$message.text());
    }
  },
  get: function() {
    if (window.localStorage) {
      return window.localStorage.getItem('writing');
    }
  },
  stateFromStore: function() {
    var s;
    s = {
      audi: StationStore.getAudience(),
      ludi: MessageStore.getLastAudience(),
      config: StationStore.getConfigs(),
      members: StationStore.getMembers(),
      typing: StationStore.getTyping(),
      station: StationStore.getStation(),
      valid: StationStore.getValidAudience()
    };
    if (s.audi.length > 1) {
      s.audi = _.without(s.audi, util.mainStationPath(window.urb.user));
    }
    if (s.ludi.length > 1) {
      s.ludi = _.without(s.ludi, util.mainStationPath(window.urb.user));
    }
    return s;
  },
  getInitialState: function() {
    return _.extend(this.stateFromStore(), {
      length: 0,
      lengthy: false
    });
  },
  typing: function(state) {
    if (this.state.typing[this.state.station] !== state) {
      return StationActions.setTyping(this.state.station, state);
    }
  },
  onBlur: function() {
    this.$message.text(this.$message.text());
    MessageActions.setTyping(false);
    return this.typing(false);
  },
  onFocus: function() {
    MessageActions.setTyping(true);
    this.typing(true);
    return this.cursorAtEnd;
  },
  addCC: function(audi) {
    var listening, ref1, ref2;
    if (urb.user !== urb.ship) {
      return audi;
    }
    listening = (ref1 = (ref2 = this.state.config[this.props.station]) != null ? ref2.src : void 0) != null ? ref1 : [];
    if (_.isEmpty(_.intersection(audi, listening))) {
      audi.push("~" + window.urb.user + "/" + this.props.station);
    }
    return audi;
  },
  sendMessage: function() {
    var audi, txt;
    if (this._validateAudi() === false) {
      setTimeout((function() {
        return $('#audience .input').focus();
      }), 0);
      return;
    }
    if (!(this.state.audi.length === 0 && $('#audience').text().trim().length > 0)) {
      audi = this.state.audi;
    } else {
      audi = this._setAudi() || this.state.ludi;
    }
    if (_.isEmpty(audi)) {
      console.warn("No audience");
      return;
    }
    if (this.props['audience-lock'] != null) {
      audi = ["~" + window.urb.ship + "/" + this.props.station];
    }
    audi = this.addCC(audi);
    txt = this.$message.text().trim().replace(/\xa0/g, ' ');
    MessageActions.sendMessage(txt, audi);
    this.$message.text('');
    this.setState({
      length: 0
    });
    this.set();
    return this.typing(false);
  },
  onKeyUp: function(e) {
    if (!window.urb.util.isURL(this.$message.text())) {
      return this.setState({
        lengthy: this.$message.text().length > 62
      });
    }
  },
  onKeyDown: function(e) {
    var txt;
    if (e.keyCode === 13) {
      txt = this.$message.text();
      e.preventDefault();
      if (txt.length > 0) {
        if (window.talk.online) {
          this.sendMessage();
        } else {
          $('#offline').addClass('error').one('transitionend', function() {
            return $('#offline').removeClass('error');
          });
        }
      }
      return false;
    }
    if (e.keyCode === 9) {
      e.preventDefault();
      this._autoComplete();
      return false;
    } else if ((this.tabList != null) && e.keyCode !== 16) {
      this.tabList = null;
      this.tabIndex = null;
    }
    this.onInput();
    return this.set();
  },
  _autoComplete: function() {
    var i, msg, name, obj, ptxt, ref1, ref2, tindex, txt;
    txt = this.$message.text();
    tindex = txt.lastIndexOf('~');
    if (tindex === -1) {
      return;
    }
    if (this.tabList == null) {
      ptxt = txt.substr(tindex + 1);
      if (ptxt.length < 13 && (ptxt.match('^[a-z]{0,6}([\\-\\^_][a-z]{0,5})?$') != null)) {
        this.tabList = [];
        ref1 = MessageStore.getAll();
        for (i = ref1.length - 1; i >= 0; i += -1) {
          msg = ref1[i];
          this._processAutoCompleteName(ptxt, msg.aut);
        }
        ref2 = this.state.members[this.state.ludi[0]];
        for (name in ref2) {
          if (!hasProp.call(ref2, name)) continue;
          obj = ref2[name];
          this._processAutoCompleteName(ptxt, name.substr(1));
        }
      }
    }
    if ((this.tabList != null) && this.tabList.length > 0) {
      if (this.tabIndex != null) {
        if (event.shiftKey) {
          this.tabIndex--;
        } else {
          this.tabIndex++;
        }
        this.tabIndex = (this.tabIndex % this.tabList.length + this.tabList.length) % this.tabList.length;
      } else {
        this.tabIndex = 0;
      }
      name = this.tabList[this.tabIndex];
      this.$message.text(this.$message.text().substr(0, tindex + 1) + name);
      return this.cursorAtEnd();
    }
  },
  _processAutoCompleteName: function(ptxt, name) {
    if (name.length === 27) {
      name = name.substr(-13).replace('-', '^');
    } else if (name.length === 56) {
      name = name.substr(0, 6) + '_' + name.substr(-6);
    }
    if (name.indexOf(ptxt) === 0 && this.tabList.indexOf(name) === -1) {
      return this.tabList.push(name);
    }
  },
  onInput: function(e) {
    var length, text;
    text = this.$message.text();
    length = text.length;
    return this.setState({
      length: length
    });
  },
  _validateAudiPart: function(a) {
    var _a, ship;
    a = a.trim();
    if (a.indexOf("/") !== -1) {
      _a = a.split("/");
      if (_a[1].length === 0) {
        return false;
      }
      ship = _a[0];
    } else {
      ship = a;
    }
    return (SHIPSHAPE.test(ship)) && _.all(ship.match(/[a-z]{3}/g), function(a) {
      return -1 !== PO.indexOf(a);
    });
  },
  _validateAudi: function() {
    var v;
    v = $('#audience .input').text();
    v = v.trim();
    if (v.length === 0) {
      return true;
    }
    if (v.length < 5) {
      return false;
    }
    return _.all(v.split(/\ +/), this._validateAudiPart);
  },
  _setAudi: function() {
    var stan, valid;
    valid = this._validateAudi();
    StationActions.setValidAudience(valid);
    if (valid === true) {
      stan = $('#audience .input').text() || util.mainStationPath(window.urb.user);
      stan = (stan.split(/\ +/)).map(function(v) {
        if (v.indexOf("/") === -1) {
          v = v + "/inbox";
        }
        if (v[0] === "~") {
          return v;
        } else {
          return "~" + v;
        }
      });
      StationActions.setAudience(stan);
      return stan;
    } else {
      return false;
    }
  },
  getTime: function() {
    var d, seconds;
    d = new Date();
    seconds = d.getSeconds();
    if (seconds < 10) {
      seconds = "0" + seconds;
    }
    return "~" + d.getHours() + "." + d.getMinutes() + "." + seconds;
  },
  cursorAtEnd: function() {
    var range, selection;
    range = document.createRange();
    range.selectNodeContents(this.$message[0]);
    range.collapse(false);
    selection = window.getSelection();
    selection.removeAllRanges();
    return selection.addRange(range);
  },
  componentDidMount: function() {
    util.sendMessage = this.sendMessage;
    StationStore.addChangeListener(this._onChangeStore);
    MessageStore.addChangeListener(this._onChangeStore);
    this.$el = $(ReactDOM.findDOMNode(this));
    this.$message = $('#message .input');
    this.$message.focus();
    if (this.get()) {
      this.$message.text(this.get());
      this.onInput();
    }
    return this.interval = setInterval((function(_this) {
      return function() {
        return _this.$el.find('.time').text(_this.getTime());
      };
    })(this), 1000);
  },
  componentWillUnmount: function() {
    StationStore.removeChangeListener(this._onChangeStore);
    return clearInterval(this.interval);
  },
  _onChangeStore: function() {
    return this.setState(this.stateFromStore());
  },
  render: function() {
    var audi, iden, k, name, ship, user, v;
    user = "~" + window.urb.user;
    iden = StationStore.getMember(user);
    ship = iden ? iden.ship : user;
    name = iden ? iden.name : "";
    audi = this.state.audi.length === 0 ? this.state.ludi : this.state.audi;
    audi = util.clipAudi(audi);
    for (k in audi) {
      v = audi[k];
      if (audi[k].indexOf('~~') === 0) {
        audi[k] = v.slice(1);
      }
    }
    return div({
      className: 'writing',
      key: 'writing'
    }, React.createElement(Audience, {
      audi: audi,
      valid: this.state.valid,
      validate: this._validateAudi,
      editable: this.props['audience-lock'] == null,
      onBlur: this._setAudi
    }), div({
      className: 'message',
      id: 'message',
      key: 'message'
    }, div({
      className: 'input',
      contentEditable: true,
      onPaste: this.onInput,
      onInput: this.onInput,
      onFocus: this.onFocus,
      onBlur: this.onBlur,
      onKeyDown: this.onKeyDown,
      onKeyUp: this.onKeyUp,
      dangerouslySetInnerHTML: {
        __html: ""
      }
    })));
  }
});


},{"../actions/MessageActions.coffee":1,"../actions/StationActions.coffee":2,"../stores/MessageStore.coffee":13,"../stores/StationStore.coffee":14,"../util.coffee":15,"./MemberComponent.coffee":4,"husl":18}],9:[function(require,module,exports){
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


},{}],10:[function(require,module,exports){
var MessageListComponent, StationActions, StationComponent, TreeActions, WritingComponent, div, link, ref, util;

util = require('./util.coffee');

_.merge(window, {
  util: util,
  talk: {
    online: true
  }
});

StationActions = require('./actions/StationActions.coffee');

TreeActions = window.tree.actions;

setInterval((function() {
  window.talk.online = window.urb.poll.dely < 500;
  if (window.talk.online) {
    return $('body').removeClass('offline');
  } else {
    return $('body').addClass('offline');
  }
}), 300);

StationComponent = require('./components/StationComponent.coffee');

MessageListComponent = React.createFactory(require('./components/MessageListComponent.coffee'));

WritingComponent = React.createFactory(require('./components/WritingComponent.coffee'));

ref = React.DOM, div = ref.div, link = ref.link;

TreeActions.registerComponent("talk", React.createClass({
  displayName: "talk",
  getStation: function() {
    return this.props.station || util.defaultStation();
  },
  componentWillMount: function() {
    var station;
    if (!this.props.readonly) {
      $(window).on('scroll', util.checkScroll);
    }
    station = this.getStation();
    StationActions.listen();
    return StationActions.switchStation(station);
  },
  render: function() {
    var children, station;
    station = this.getStation();
    children = [
      div({
        key: "grams-container"
      }, MessageListComponent(_.merge({}, this.props, {
        station: station,
        key: 'grams'
      }), '')), this.props.readOnly == null ? div({
        key: 'writing-container'
      }, WritingComponent(_.merge({}, this.props, {
        station: station,
        key: 'writing'
      }), '')) : void 0
    ];
    if (this.props.chrono === "reverse") {
      children = children.reverse();
    }
    return div({
      key: "talk-container"
    }, children);
  }
}));

TreeActions.registerComponent("talk-station", StationComponent);


},{"./actions/StationActions.coffee":2,"./components/MessageListComponent.coffee":6,"./components/StationComponent.coffee":7,"./components/WritingComponent.coffee":8,"./util.coffee":15}],11:[function(require,module,exports){
var util;

util = require('../util.coffee');

window.urb.appl = "hall";

module.exports = function(arg) {
  var MessageActions;
  MessageActions = arg.MessageActions;
  return {
    listenStation: function(station, since) {
      var $this, begin, path;
      $this = this;
      begin = window.urb.util.toDate(since);
      path = util.talkPath('circle', station, 'grams', begin);
      return window.urb.bind(path, function(err, res) {
        var ref, ref1, ref2, ref3;
        if (err || !res.data) {
          console.log(path, 'err!');
          console.log(err);
          console.log(res);
          $this.listenStation(station, since);
          return;
        }
        if (res.data.ok === true) {
          MessageActions.listeningStation(station);
        }
        if ((ref = res.data) != null ? (ref1 = ref.circle) != null ? ref1.nes : void 0 : void 0) {
          if (res.data.circle.nes.length === 0) {
            window.urb.drop(path, function(err, res) {
              if (err) {
                return console.log(err);
              }
            });
            console.log('trying for older than ' + begin);
            $this.listenStation(station, new Date(since - 6 * 3600 * 1000));
          } else {
            res.data.circle.nes.map(function(env) {
              env.gam.heard = true;
              return env;
            });
            MessageActions.loadMessages(res.data.circle.nes);
          }
        }
        if ((ref2 = res.data) != null ? (ref3 = ref2.circle) != null ? ref3.gram : void 0 : void 0) {
          res.data.circle.gram.gam.heard = true;
          return MessageActions.loadMessages([res.data.circle.gram]);
        }
      });
    },
    get: function(station, start, end) {
      var path;
      end = window.urb.util.numDot(end);
      start = window.urb.util.numDot(start);
      path = util.talkPath('circle', station, 'grams', end, start);
      return window.urb.bind(path, function(err, res) {
        var ref, ref1;
        if (err || !res.data) {
          console.log(path, '/circle err');
          console.log(err);
          return;
        }
        if ((ref = res.data) != null ? (ref1 = ref.circle) != null ? ref1.nes : void 0 : void 0) {
          res.data.circle.nes.map(function(env) {
            env.gam.heard = true;
            return env;
          });
          MessageActions.loadMessages(res.data.circle.nes);
          return window.urb.drop(path, function(err, res) {
            console.log('done');
            return console.log(res);
          });
        }
      });
    },
    sendMessage: function(message, cb) {
      if (window.urb.user === window.urb.ship) {
        return window.urb.send({
          convey: [message]
        }, {
          mark: "hall-action"
        }, function(err, res) {
          console.log('sent local');
          console.log(arguments);
          if (cb) {
            return cb(err, res);
          }
        });
      } else {
        return window.urb.send({
          publish: [message]
        }, {
          mark: "hall-command"
        }, function(err, res) {
          console.log('sent remote');
          console.log(arguments);
          if (cb) {
            return cb(err, res);
          }
        });
      }
    }
  };
};


},{"../util.coffee":15}],12:[function(require,module,exports){
var create, remove, send, source, subscribed, util;

util = require('../util.coffee');

window.urb.appl = "hall";

send = function(data, cb) {
  return window.urb.send(data, {
    mark: "hall-action"
  }, cb);
};

create = function(nom, des, sec, cb) {
  return send({
    create: {
      nom: nom,
      des: des,
      sec: sec
    }
  }, cb);
};

remove = function(nom, why, cb) {
  return send({
    "delete": {
      nom: nom,
      why: why
    }
  }, cb);
};

source = function(nom, sub, srs, cb) {
  return send({
    source: {
      nom: nom,
      sub: sub,
      srs: srs
    }
  }, cb);
};

subscribed = {};

module.exports = function(arg) {
  var StationActions;
  StationActions = arg.StationActions;
  return {
    createStation: function(name, cb) {
      return create(name, "", "black", cb);
    },
    removeStation: function(name, cb) {
      return remove(name, 'deleted through webtalk', cb);
    },
    modSources: function(station, sub, sources) {
      return source(station, sub, sources, function(err, res) {
        console.log('hall-action source');
        return console.log(arguments);
      });
    },
    addSources: function(station, sources) {
      return this.modSources(station, true, sources);
    },
    remSources: function(station, sources) {
      return this.modSources(station, false, sources);
    },
    listen: function() {
      var date;
      date = window.urb.util.toDate(new Date());
      return window.urb.bind('/client', function(err, res) {
        var gys, nis, ref;
        if (err || !res.data) {
          console.log('sp err');
          console.log(err);
          return;
        }
        ref = res.data.client, gys = ref.gys, nis = ref.nis;
        return StationActions.loadGlyphs(gys);
      });
    },
    listenStation: function(station) {
      var path;
      if (subscribed[station] == null) {
        subscribed[station] = {};
      }
      path = util.talkPath('circle', station, 'config-l', 'group-r', '0');
      return window.urb.bind(path, function(err, res) {
        var config, cos, pes, ref, status;
        if (err || !res) {
          console.log(path, 'err');
          console.log(err);
          return;
        }
        ref = res.data.circle, cos = ref.cos, pes = ref.pes, config = ref.config, status = ref.status;
        if (res.data.ok) {
          StationActions.listeningStation(station);
        }
        switch (false) {
          case !cos:
            return StationActions.loadConfig(station, cos.loc);
          case !pes:
            return StationActions.loadMembers(station, pes.loc);
          case !config:
            if (config.dif.source != null) {
              if (config.dif.source.add) {
                return StationActions.addStation(config.dif.source.src);
              } else {
                return StationActions.remStation(config.dif.source.src);
              }
            }
            break;
          case !status:
            break;
        }
      });
    }
  };
};


},{"../util.coffee":15}],13:[function(require,module,exports){
var EventEmitter, MessageDispatcher, MessageStore, _fetching, _filter, _listening, _messages, _oldest, _station, _typing, moment;

moment = window.moment.tz;

EventEmitter = require('events').EventEmitter;

MessageDispatcher = require('../dispatcher/Dispatcher.coffee');

_messages = {};

_fetching = false;

_oldest = null;

_station = null;

_filter = null;

_listening = [];

_typing = false;

MessageStore = _.merge(new EventEmitter, {
  removeChangeListener: function(cb) {
    return this.removeListener("change", cb);
  },
  emitChange: function() {
    return this.emit('change');
  },
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  leadingZero: function(str) {
    if (Number(str) < 10) {
      return "0" + str;
    } else {
      return str;
    }
  },
  convertDate: function(time) {
    var d;
    time = time.substr(1).split(".");
    time[1] = this.leadingZero(time[1]);
    time[2] = this.leadingZero(time[2]);
    d = new moment(time[0] + "-" + time[1] + "-" + time[2] + "T" + time[4] + ":" + time[5] + ":" + time[6] + "Z");
    d.tz("Europe/London");
    return d;
  },
  getListening: function() {
    return _listening;
  },
  getTyping: function() {
    return _typing;
  },
  getLastAudience: function() {
    var messages;
    if (_.keys(_messages).length === 0) {
      return [];
    }
    messages = _.sortBy(_messages, function(_message) {
      return _message.wen;
    });
    return messages[messages.length - 1].aud;
  },
  setTyping: function(state) {
    return _typing = state;
  },
  setListening: function(station) {
    if (_listening.indexOf(station) !== -1) {
      return console.log('already listening on that station (somehow).');
    } else {
      return _listening.push(station);
    }
  },
  setStation: function(station) {
    return _station = station;
  },
  getFilter: function() {
    return _filter;
  },
  setFilter: function(station) {
    return _filter = station;
  },
  clearFilter: function(station) {
    return _filter = null;
  },
  sendMessage: function(message) {
    return _messages[message.uid] = message;
  },
  loadMessages: function(messages, get) {
    var i, len, min, serial, v;
    min = _oldest;
    for (i = 0, len = messages.length; i < len; i++) {
      v = messages[i];
      v.gam.key = v.num;
      if (v.num < min || min === null) {
        min = v.num;
      }
      v = v.gam || v;
      serial = v.uid;
      _messages[serial] = v;
    }
    if (min < _oldest || _oldest === null || get === true) {
      _oldest = min;
    }
    return _fetching = false;
  },
  getAll: function() {
    var mess;
    mess = _.values(_messages);
    if (!_filter) {
      return mess;
    } else {
      return _.filter(mess, function(mess) {
        return mess.aud.indexOf(_filter) !== -1;
      });
    }
  },
  getFetching: function() {
    return _fetching;
  },
  setFetching: function(state) {
    return _fetching = state;
  },
  getOldest: function() {
    return _oldest;
  }
});

MessageStore.setMaxListeners(100);

MessageStore.dispatchToken = MessageDispatcher.register(function(payload) {
  var action;
  action = payload.action;
  switch (action.type) {
    case 'station-switch':
      MessageStore.setStation(action.station);
      break;
    case 'messages-filter':
      MessageStore.setFilter(action.station);
      MessageStore.emitChange();
      break;
    case 'messages-filter-clear':
      MessageStore.clearFilter(action.station);
      MessageStore.emitChange();
      break;
    case 'messages-listen':
      MessageStore.setListening(action.station);
      MessageStore.emitChange();
      break;
    case 'messages-typing':
      MessageStore.setTyping(action.state);
      MessageStore.emitChange();
      break;
    case 'messages-fetch':
      MessageStore.setFetching(true);
      MessageStore.emitChange();
      break;
    case 'messages-load':
      MessageStore.loadMessages(action.messages, action.last, action.get);
      MessageStore.emitChange();
      break;
    case 'message-load':
      MessageStore.loadMessage(action.time, action.message, action.author);
      MessageStore.emitChange();
      break;
    case 'message-send':
      MessageStore.sendMessage(action.message);
      MessageStore.emitChange();
      break;
  }
});

module.exports = MessageStore;


},{"../dispatcher/Dispatcher.coffee":9,"events":17}],14:[function(require,module,exports){
var EventEmitter, StationDispatcher, StationStore, _audience, _config, _glyphs, _listening, _members, _shpylg, _station, _stations, _typing, _validAudience;

EventEmitter = require('events').EventEmitter;

StationDispatcher = require('../dispatcher/Dispatcher.coffee');

_audience = [];

_members = {};

_stations = [];

_listening = [];

_station = null;

_config = {};

_typing = {};

_glyphs = {};

_shpylg = {};

_validAudience = true;

StationStore = _.merge(new EventEmitter, {
  removeChangeListener: function(cb) {
    return this.removeListener("change", cb);
  },
  emitChange: function() {
    return this.emit('change');
  },
  addChangeListener: function(cb) {
    return this.on('change', cb);
  },
  getAudience: function() {
    return _audience;
  },
  setAudience: function(audience) {
    return _audience = audience;
  },
  getValidAudience: function() {
    return _validAudience;
  },
  setValidAudience: function(valid) {
    return _validAudience = valid;
  },
  toggleAudience: function(station) {
    if (_audience.indexOf(station) !== -1) {
      return _audience.splice(_audience.indexOf(station), 1);
    } else {
      return _audience.push(station);
    }
  },
  loadConfig: function(station, config) {
    return _config[station] = config;
  },
  getConfigs: function() {
    return _config;
  },
  getConfig: function(station) {
    return _config[station];
  },
  getGlyph: function(station) {
    return _shpylg[station];
  },
  getGlyphMap: function() {
    return _shpylg;
  },
  getGlyphAudience: function(glyph) {
    var aud, ref;
    aud = (ref = _glyphs[glyph]) != null ? ref : [];
    if (aud.length === 1) {
      return aud[0];
    }
  },
  getMember: function(ship) {
    return {
      ship: ship
    };
  },
  loadMembers: function(station, members) {
    return _members[station] = members;
  },
  getMembers: function(station) {
    return _members[station];
  },
  getListening: function() {
    return _listening;
  },
  setListening: function(station) {
    if (_listening.indexOf(station) !== -1) {
      return console.log('already listening on that station (somehow).');
    } else {
      return _listening.push(station);
    }
  },
  createStation: function(station) {
    if (_stations.indexOf(station) === -1) {
      return _stations.push(station);
    }
  },
  removeStation: function(station) {
    var i;
    i = _stations.indexOf(station);
    if (i > -1) {
      return _stations.splice(i, 1);
    }
  },
  loadStations: function(stations) {
    return _stations = stations;
  },
  loadGlyphs: function(glyphs) {
    var aud, auds, char, results;
    _glyphs = glyphs;
    _shpylg = {};
    results = [];
    for (char in glyphs) {
      auds = glyphs[char];
      results.push((function() {
        var j, len, results1;
        results1 = [];
        for (j = 0, len = auds.length; j < len; j++) {
          aud = auds[j];
          results1.push(_shpylg[aud.join(" ")] = char);
        }
        return results1;
      })());
    }
    return results;
  },
  getGlyphs: function() {
    return _glyphs;
  },
  getStations: function() {
    return _stations;
  },
  setStation: function(station) {
    return _station = station;
  },
  unsetStation: function(station) {
    if (_station === station) {
      return _station = null;
    }
  },
  getStation: function() {
    return _station;
  },
  joinStation: function(station) {
    var ref;
    if (((ref = _config.court) != null ? ref.sources.indexOf(station) : void 0) === -1) {
      return _config.court.sources.push(station);
    }
  },
  getTyping: function() {
    return _typing;
  },
  setTyping: function(station, state) {
    var k, v;
    for (k in _typing) {
      v = _typing[k];
      _typing[k] = k === station;
    }
    return _typing[station] = state;
  }
});

StationStore.setMaxListeners(100);

StationStore.dispatchToken = StationDispatcher.register(function(payload) {
  var action;
  action = payload.action;
  switch (action.type) {
    case 'station-audience-toggle':
      StationStore.toggleAudience(action.station);
      StationStore.emitChange();
      break;
    case 'station-set-audience':
      StationStore.setAudience(action.audience);
      StationStore.emitChange();
      break;
    case 'station-set-valid-audience':
      StationStore.setValidAudience(action.valid);
      StationStore.emitChange();
      break;
    case 'station-switch':
      StationStore.setAudience([]);
      StationStore.setStation(action.station);
      StationStore.emitChange();
      break;
    case 'station-listen':
      StationStore.setListening(action.station);
      StationStore.emitChange();
      break;
    case "config-load":
      StationStore.loadConfig(action.station, action.config);
      StationStore.emitChange();
      break;
    case "glyphs-load":
      StationStore.loadGlyphs(action.glyphs);
      StationStore.emitChange();
      break;
    case "stations-load":
      StationStore.loadStations(action.stations);
      StationStore.emitChange();
      break;
    case "stations-leave":
      StationStore.loadStations(action.stations);
      StationStore.unsetStation(action.station);
      StationStore.emitChange();
      break;
    case "station-create":
      StationStore.createStation(action.station);
      StationStore.emitChange();
      break;
    case "station-remove":
      StationStore.removeStation(action.station);
      StationStore.emitChange();
      break;
    case "members-load":
      StationStore.loadMembers(action.members);
      StationStore.emitChange();
      break;
    case "typing-set":
      StationStore.setTyping(action.station, action.state);
      StationStore.emitChange();
      break;
  }
});

module.exports = StationStore;


},{"../dispatcher/Dispatcher.coffee":9,"events":17}],15:[function(require,module,exports){
var util,
  slice = [].slice;

module.exports = util = {
  defaultStation: function() {
    var station;
    if (document.location.search) {
      station = document.location.search.replace(/^\?/, '');
      if (station.indexOf('dbg.nopack') !== -1) {
        return station = util.mainStation();
      }
    } else {
      return util.mainStation();
    }
  },
  mainStationPath: function(user) {
    if (user) {
      return "~" + user + "/inbox";
    }
  },
  mainStation: function(user) {
    return "inbox";
  },
  clipAudi: function(audi) {
    var ms, regx;
    audi = audi.join(" ");
    ms = util.mainStationPath(window.urb.user);
    regx = new RegExp("/" + ms, "g");
    audi = audi.replace(regx, "");
    return audi.split(" ");
  },
  expandAudi: function(audi) {
    var ms;
    audi = audi.join(" ");
    ms = util.mainStationPath(window.urb.user);
    if (audi.indexOf(ms) === -1) {
      if (audi.length > 0) {
        audi += " ";
      }
      audi += "" + ms;
    }
    return audi.split(" ");
  },
  create: function(name) {
    return window.talk.StationPersistence.createStation(name, function(err, res) {});
  },
  subscribe: function(name) {
    return window.talk.StationPersistence.addSource("main", window.urb.ship, ["~zod/" + name]);
  },
  uuid32: function() {
    var _str, i, j, str;
    str = "0v";
    str += Math.ceil(Math.random() * 8) + ".";
    for (i = j = 0; j <= 5; i = ++j) {
      _str = Math.ceil(Math.random() * 10000000).toString(32);
      _str = ("00000" + _str).substr(-5, 5);
      str += _str + ".";
    }
    return str.slice(0, -1);
  },
  populate: function(station, number) {
    var c, send;
    c = 0;
    send = function() {
      var _audi, _message;
      if (c < number) {
        c++;
      } else {
        console.log('done');
        return true;
      }
      _audi = {};
      _audi[station] = "pending";
      _message = {
        serial: util.uuid32(),
        audience: _audi,
        statement: {
          speech: {
            say: "Message " + c
          },
          time: Date.now(),
          now: Date.now()
        }
      };
      return window.talk.MessagePersistence.sendMessage(_message, send);
    };
    return send();
  },
  scrollToBottom: function() {
    return $(window).scrollTop($(".container").outerHeight(true));
  },
  getScroll: function() {
    return this.writingPosition = $('.container').outerHeight(true) + $('.container').offset().top - $(window).height();
  },
  setScroll: function() {
    util.getScroll();
    return $(window).scrollTop($(".container").height());
  },
  isScrolling: function() {
    if (!util.writingPosition) {
      util.getScroll();
    }
    return $(window).scrollTop() + $('.writing').outerHeight() < util.writingPosition;
  },
  talkPath: function() {
    var components;
    components = 1 <= arguments.length ? slice.call(arguments, 0) : [];
    return [''].concat(slice.call(components)).join('/');
  }
};


},{}],16:[function(require,module,exports){
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

},{}],18:[function(require,module,exports){
(function(){function b(){}function h(){}function n(){}function k(){}function f(){}function e(a){return function(b,c,l){return a([b,c,l])}}f.j=function(a){a=a.charCodeAt(1);if(a==a)return a};f.substr=function(a,b,c){if(null==c)c=a.length;else if(0>c)if(0==b)c=a.length+c;else return"";return a.substr(b,c)};k.parseInt=function(a){var b=parseInt(a,10);0!=b||120!=f.j(a)&&88!=f.j(a)||(b=parseInt(a));return isNaN(b)?null:b};n.L=function(a){for(var b="";b="0123456789ABCDEF".charAt(a&15)+b,a>>>=4,0<a;);for(;2>
b.length;)b="0"+b;return b};h.K=function(a){return Math.abs(a.v)/Math.sqrt(Math.pow(a.J,2)+1)};h.X=function(a,b){return b.v/(Math.sin(a)-b.J*Math.cos(a))};b.m=function(a){for(var d=[],c=Math.pow(a+16,3)/1560896,c=c>b.l?c:a/b.g,l=0;3>l;)for(var g=l++,p=b.b[g][0],f=b.b[g][1],g=b.b[g][2],e=0;2>e;){var h=e++,k=(632260*g-126452*f)*c+126452*h;d.push({J:(284517*p-94839*g)*c/k,v:((838422*g+769860*f+731718*p)*a*c-769860*h*a)/k})}return d};b.B=function(a){a=b.m(a);for(var d=1.7976931348623157E308,c=0;2>c;)d=
Math.min(d,h.K(a[c++]));return d};b.A=function(a,d){for(var c=d/360*Math.PI*2,l=b.m(a),g=1.7976931348623157E308,f=0;f<l.length;){var e=l[f];++f;e=h.X(c,e);0<=e&&(g=Math.min(g,e))}return g};b.a=function(a,b){for(var c=0,d=0,f=a.length;d<f;)var e=d++,c=c+a[e]*b[e];return c};b.f=function(a){return.0031308>=a?12.92*a:1.055*Math.pow(a,.4166666666666667)-.055};b.i=function(a){return.04045<a?Math.pow((a+.055)/1.055,2.4):a/12.92};b.ba=function(a){return[b.f(b.a(b.b[0],a)),b.f(b.a(b.b[1],a)),b.f(b.a(b.b[2],
a))]};b.$=function(a){a=[b.i(a[0]),b.i(a[1]),b.i(a[2])];return[b.a(b.h[0],a),b.a(b.h[1],a),b.a(b.h[2],a)]};b.ca=function(a){return a<=b.l?a/b.c*b.g:116*Math.pow(a/b.c,.3333333333333333)-16};b.T=function(a){return 8>=a?b.c*a/b.g:b.c*Math.pow((a+16)/116,3)};b.aa=function(a){var d=a[0],c=a[1];a=d+15*c+3*a[2];0!=a?(d=4*d/a,a=9*c/a):a=d=NaN;c=b.ca(c);return 0==c?[0,0,0]:[c,13*c*(d-b.C),13*c*(a-b.D)]};b.Z=function(a){var d=a[0];if(0==d)return[0,0,0];var c=a[1]/(13*d)+b.C;a=a[2]/(13*d)+b.D;d=b.T(d);c=0-
9*d*c/((c-4)*a-c*a);return[c,d,(9*d-15*a*d-a*c)/(3*a)]};b.Y=function(a){var b=a[0],c=a[1],e=a[2];a=Math.sqrt(c*c+e*e);1E-8>a?c=0:(c=180*Math.atan2(e,c)/3.141592653589793,0>c&&(c=360+c));return[b,a,c]};b.W=function(a){var b=a[1],c=a[2]/360*2*Math.PI;return[a[0],Math.cos(c)*b,Math.sin(c)*b]};b.P=function(a){var d=a[0],c=a[1];a=a[2];return 99.9999999<a?[100,0,d]:1E-8>a?[0,0,d]:[a,b.A(a,d)/100*c,d]};b.U=function(a){var d=a[0],c=a[2];return 99.9999999<d?[c,0,100]:1E-8>d?[c,0,0]:[c,a[1]/b.A(d,c)*100,d]};
b.S=function(a){var d=a[0],c=a[1];a=a[2];return 99.9999999<a?[100,0,d]:1E-8>a?[0,0,d]:[a,b.B(a)/100*c,d]};b.V=function(a){var d=a[0],c=a[2];return 99.9999999<d?[c,0,100]:1E-8>d?[c,0,0]:[c,a[1]/b.B(d)*100,d]};b.F=function(a){for(var b="#",c=0,e=a.length;c<e;)b+=n.L(Math.round(255*a[c++])).toLowerCase();return b};b.o=function(a){a=a.toUpperCase();return[k.parseInt("0x"+f.substr(a,1,2))/255,k.parseInt("0x"+f.substr(a,3,2))/255,k.parseInt("0x"+f.substr(a,5,2))/255]};b.w=function(a){return b.ba(b.Z(b.W(a)))};
b.I=function(a){return b.Y(b.aa(b.$(a)))};b.s=function(a){return b.w(b.P(a))};b.G=function(a){return b.U(b.I(a))};b.u=function(a){return b.w(b.S(a))};b.H=function(a){return b.V(b.I(a))};b.O=function(a){return b.F(b.s(a))};b.R=function(a){return b.F(b.u(a))};b.M=function(a){return b.G(b.o(a))};b.N=function(a){return b.H(b.o(a))};b.b=[[3.240969941904521,-1.537383177570093,-.498610760293],[-.96924363628087,1.87596750150772,.041555057407175],[.055630079696993,-.20397695888897,1.056971514242878]];b.h=
[[.41239079926595,.35758433938387,.18048078840183],[.21263900587151,.71516867876775,.072192315360733],[.019330818715591,.11919477979462,.95053215224966]];b.c=1;b.C=.19783000664283;b.D=.46831999493879;b.g=903.2962962;b.l=.0088564516;var m={fromRGB:e(b.G),fromHex:b.M,toRGB:e(b.s),toHex:e(b.O),p:{fromRGB:e(b.H),fromHex:b.N,toRGB:e(b.u),toHex:e(b.R)}};"undefined"!==typeof jQuery&&(jQuery.husl=m);"undefined"!==typeof module&&(module.exports=m);"undefined"!==typeof define&&define(m);"undefined"!==typeof window&&
(window.HUSL=m)})();

},{}]},{},[10]);
