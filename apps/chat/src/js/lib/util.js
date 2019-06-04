import _ from 'lodash';
import classnames from 'classnames';


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

export function deSig(ship) {
  return ship.replace('~', '');
}

// shorten comet names
export function prettyShip(ship) {
  const sp = ship.split('-');
  return [sp.length == 9 ? `${sp[0]}_${sp[8]}`: ship, ship[0] === '~' ? `/~profile/${ship}` : `/~profile/~${ship}`];
}

export function profileUrl(ship) {
  return `/~landscape/profile/~${ship}`;
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
    console.error("ASSERT: unknown message type on ", msg)
  }

  return ret;
}

window.getMessageContent = getMessageContent;
