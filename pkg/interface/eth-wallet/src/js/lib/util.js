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

export function getContactDetails(contact) {
  contact = contact || {
    'nickname': '',
    'avatar': 'TODO',
    'color': '0x0'
  };
  const nickname = contact.nickname || '';
  const color = uxToHex(contact.color || '0x0');
  return {nickname, color};
}

// encodes string into base64url,
// by encoding into base64 and replacing non-url-safe characters.
//
export function base64urlEncode(string) {
  return window.btoa(string)
    .split('+').join('-')
    .split('/').join('_');
}

// decode base64url. inverse of base64urlEncode above.
//
export function base64urlDecode(string) {
  return window.atob(
    string.split('_').join('/')
          .split('-').join('+')
  );
}

export function isPatTa(str) {
  const r = /^[a-z,0-9,\-,\.,_,~]+$/.exec(str)
  return !!r;
}

// encode the string into @ta-safe format, using logic from +wood.
// for example, 'some Chars!' becomes '~.some.~43.hars~21.'
//
export function stringToTa(string) {
  let out = '';
  for (let i = 0; i < string.length; i++) {
    const char = string[i];
    let add = '';
    switch (char) {
      case ' ':
        add = '.';
        break;
      case '.':
        add = '~.';
        break;
      case '~':
        add = '~~';
        break;
      default:
        const charCode = string.charCodeAt(i);
        if (
          (charCode >= 97 && charCode <= 122) || // a-z
          (charCode >= 48 && charCode <= 57)  || // 0-9
          char === '-'
        ) {
          add = char;
        } else {
          //TODO behavior for unicode doesn't match +wood's,
          //     but we can probably get away with that for now.
          add = '~' + charCode.toString(16) + '.';
        }
    }
    out = out + add;
  }
  return '~.' + out;
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

export function uxToHex(ux) {
  if (ux.length > 2 && ux.substr(0,2) === '0x') {
    return ux.substr(2).replace('.', '').padStart(6, '0');
  } else {
    return ux.replace('.', '').padStart(6, '0');
  }
}
