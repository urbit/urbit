import _ from 'lodash';
import urbitOb from 'urbit-ob';
import classnames from 'classnames';

export const AGGREGATOR_COLL = "c";
export const AGGREGATOR_INBOX = "aggregator-inbox";
export const AGGREGATOR_NAMES = [AGGREGATOR_INBOX, AGGREGATOR_COLL];

export function capitalize(str) {
  return `${str[0].toUpperCase()}${str.substr(1)}`;
}

// takes a galactic (urbit) time and converts to 8601
export function esoo(str) {

  var dubb = function(num) {
    return num < 10 ? '0' + parseInt(num) : parseInt(num);
  }

  const p = /\~(\d\d\d\d).(\d\d?).(\d\d?)..(\d\d?).(\d\d?).(\d\d?)/.exec(str);

  if (p) {
    return `${p[1]}-${dubb(p[2])}-${dubb(p[3])}T${dubb(p[4])}:${dubb(p[5])}:${dubb(p[6])}Z`
  }
  return false;

}

// check if hostname follows ship.*.urbit.org scheme
export function isProxyHosted(hostName) {
  const r = /([a-z,-]+)\.(.+\.)?urbit\.org/.exec(hostName);
  if (r && urbitOb.isValidPatp(r[1])) {
    return true;
  }
  return false;
}

export function getQueryParams() {
  if (window.location.search !== "") {
    return JSON.parse('{"' + decodeURI(window.location.search.substr(1).replace(/&/g, "\",\"").replace(/=/g,"\":\"")) + '"}');
  } else {
    return {};
  }
}

export function isAggregator(station) {
  let cir = station.split("/")[1]
  return AGGREGATOR_NAMES.includes(cir);
}

/*
  Goes from:
    1531943107869               // "javascript unix time"
  To:
    "48711y 2w 5d 11m 9s"       // "stringified time increments"
*/

export function secToString(secs) {
  if (secs <= 0) {
    return 'Completed';
  }
  secs = Math.floor(secs)
  var min = 60;
  var hour = 60 * min;
  var day = 24 * hour;
  var week = 7 * day;
  var year = 52 * week;
  var fy = function(s) {
    if (s < year) {
      return ['', s];
    } else {
      return [Math.floor(s / year) + 'y', s % year];
    }
  }
  var fw = function(tup) {
    var str = tup[0];
    var sec = tup[1];
    if (sec < week) {
      return [str, sec];
    } else {
      return [str + ' ' + Math.floor(sec / week) + 'w', sec % week];
    }
  }
  var fd = function(tup) {
    var str = tup[0];
    var sec = tup[1];
    if (sec < day) {
      return [str, sec];
    } else {
      return [str + ' ' + Math.floor(sec / day) + 'd', sec % day];
    }
  }
  var fh = function(tup) {
    var str = tup[0];
    var sec = tup[1];
    if (sec < hour) {
      return [str, sec];
    } else {
      return [str + ' ' + Math.floor(sec / hour) + 'h', sec % hour];
    }
  }
  var fm = function(tup) {
    var str = tup[0];
    var sec = tup[1];
    if (sec < min) {
      return [str, sec];
    } else {
      return [str + ' ' + Math.floor(sec / min) + 'm', sec % min];
    }
  }
  var fs = function(tup) {
    var str = tup[0];
    var sec = tup[1];
    return str + ' ' + sec + 's';
  }
  return fs(fm(fh(fd(fw(fy(secs)))))).trim();
}

export function uuid() {
  let str = "0v"
  str += Math.ceil(Math.random()*8)+"."
  for (var i = 0; i < 5; i++) {
    let _str = Math.ceil(Math.random()*10000000).toString(32);
    _str = ("00000"+_str).substr(-5,5);
    str += _str+".";
  }

  return str.slice(0,-1);
}

export function isPatTa(str) {
  const r = /^[a-z,0-9,\-,\.,_,~]+$/.exec(str)
  return !!r;
}

export function isValidStation(st) {
  let tokens = st.split("/")

  if (tokens.length !== 2) return false;

  return urbitOb.isValidPatp(tokens[0]) && isPatTa(tokens[1]);
}


/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st) {
  var dub = function(n) {
    return parseInt(n) < 10 ? "0" + parseInt(n) : n.toString();
  };
  var da = st.split('..');
  var bigEnd = da[0].split('.');
  var lilEnd = da[1].split('.');
  var ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(lilEnd[0])}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

/*
  Goes from:
    (javascript Date object)
  To:
    ~2018.7.17..23.15.09..5be5    // urbit @da
*/

export function dateToDa(d, mil) {
  var fil = function(n) {
    return n >= 10 ? n : "0" + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${(d.getUTCMonth() + 1)}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? "..0000" : ""}`
  );
}

  // ascending for clarity
// export function sortSrc(circleArray, topic=false){
//   let sc = circleArray.map((c) => util.parseCollCircle(c)).filter((pc) => typeof pc != 'undefined' && typeof pc.top == 'undefined');
//   return sc.map((src) => src.coll).sort((a, b) => util.daToDate(a) - util.daToDate(b));
// }

export function arrayEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (a.length != b.length) return false;

  // If you don't care about the order of the elements inside
  // the array, you should sort both arrays here.

  for (var i = 0; i < a.length; ++i) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

function deSig(ship) {
  return ship.replace('~', '');
}

// use urbit.org proxy if it's not on our ship
export function foreignUrl(shipName, own, urlFrag) {
  if (deSig(shipName) != deSig(own)) {
    return `http://${deSig(shipName)}.urbit.org${urlFrag}`
  } else {
    return urlFrag
  }
}

// shorten comet names
export function prettyShip(ship) {
  const sp = ship.split('-');
  return [sp.length == 9 ? `${sp[0]}_${sp[8]}`: ship, ship[0] === '~' ? `/~profile/${ship}` : `/~profile/~${ship}`];
}

export function profileUrl(ship) {
  return `/~landscape/profile/~${ship}`;
}

export function isDMStation(station) {
  let host = station.split('/')[0].substr(1);
  let circle = station.split('/')[1];

  return (
    station.indexOf('.') !== -1 &&
    circle.indexOf(host) !== -1
  );
}

export function isRootCollection(station) {
  return station.split("/")[1] === "c";
}

// maybe do fancier stuff later
export function isUrl(string) {
  const r = /^http|^www|\.com$/.exec(string)
  if (r) {
    return true
  }
  else {
    return false
  }
}

export function arrayify(obj) {
  let ret = [];
  Object.keys(obj).forEach((key) => {
    ret.push({key, value: obj[key]});
  })

  return ret;
}

export function getMessageContent(msg) {
  let ret;

  const MESSAGE_TYPES = {
    'sep.app.sep.fat.sep.lin.msg': 'app',
    'sep.app.sep.lin.msg': 'app',
    'sep.app.sep.inv': (msg) => {
      let sta = msg.sep.app.sep.inv.cir;
      let [hos, cir] = sta.split('/');

      return {
        type: 'inv',
        msg: msg,
        content: {
          nom: msg.sep.app.app,
          sta: sta,
          hos: hos,
          inv: msg.sep.app.sep.inv.inv
        }
      }
    },
    'sep.inv': (msg) => {
      let sta = msg.sep.inv.cir;
      let [hos, cir] = sta.split('/');

      return {
        type: 'inv',
        msg: msg,
        content: {
          nom: cir,
          inv: msg.sep.inv.inv,
          hos,
          sta,
          cir
        }
      }
    },
    'sep.fat': (msg) => {
      let type = msg.sep.fat.tac.text;
      let station = msg.aud[0];
      let jason = JSON.parse(msg.sep.fat.sep.lin.msg);
      let content = (type.includes('collection')) ? null : jason.content;
      let par = jason.path.slice(0, -1);

      return {
        type: msg.sep.fat.tac.text,
        msg: msg,
        contentType: jason.type,
        content: content,
        snip: jason.snip,
        author: jason.author,
        host: jason.host,
        date: jason.date,
        path: jason.path,
        postTitle: jason.name,
        postUrl: `/~landscape/collections/${jason.host}/${jason.path.slice(2).join('/')}`,
      }
    },
    'sep.lin.msg': 'lin',
    'sep.ire.sep.lin': (msg) => {
      return {
        type: "lin",
        msg: msg,
        content: msg.sep.ire.sep.lin.msg,
        replyUid: msg.sep.ire.top
      }
    },
    'sep.ire': 'ire',
    'sep.url': 'url',
    'sep.exp': (msg) => {
      return {
        type: "exp",
        msg: msg,
        content: msg.sep.exp.exp,
        res: msg.sep.exp.res.join('\n')
      }
    },
  }

  arrayify(MESSAGE_TYPES).some(({key, value}) => {
    if (_.has(msg, key)) {
      if (typeof value === "string") {
        ret = {
          type: value,
          msg: msg,
          content: _.get(msg, key)
        }
      } else if (typeof value === "function") {
        ret = value(msg);
      }
      return true;
    }
  });

  if (typeof ret === "undefined") {
    ret = {type: "unknown"};
    console.log("ASSERT: unknown message type on ", msg)
  }

  return ret;
}

window.getMessageContent = getMessageContent;
