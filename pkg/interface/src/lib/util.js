import _ from 'lodash';

export function resourceAsPath(resource) {
  const { name, ship } = resource;
  return `/ship/~${ship}/${name}`;
}

export function uuid() {
  let str = '0v';
  str += Math.ceil(Math.random()*8)+'.';
  for (let i = 0; i < 5; i++) {
    let _str = Math.ceil(Math.random()*10000000).toString(32);
    _str = ('00000'+_str).substr(-5,5);
    str += _str+'.';
  }

  return str.slice(0,-1);
}

export function isPatTa(str) {
  const r = /^[a-z,0-9,\-,\.,_,~]+$/.exec(str);
  return Boolean(r);
}

/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st) {
  const dub = function(n) {
    return parseInt(n) < 10 ? '0' + parseInt(n) : n.toString();
  };
  const da = st.split('..');
  const bigEnd = da[0].split('.');
  const lilEnd = da[1].split('.');
  const ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(lilEnd[0])}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

/*
  Goes from:
    (javascript Date object)
  To:
    ~2018.7.17..23.15.09..5be5    // urbit @da
*/

export function dateToDa(d, mil) {
  const fil = function(n) {
    return n >= 10 ? n : '0' + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${(d.getUTCMonth() + 1)}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? '..0000' : ''}`
  );
}

export function deSig(ship) {
  return ship.replace('~', '');
}

export function uxToHex(ux) {
  if (ux.length > 2 && ux.substr(0,2) === '0x') {
    const value = ux.substr(2).replace('.', '').padStart(6, '0');
    return value;
  }

  const value = ux.replace('.', '').padStart(6, '0');
  return value;
}

function hexToDec(hex) {
  const alphabet = '0123456789ABCDEF'.split('');
  return hex.reverse().reduce((acc, digit, idx) => {
    const dec = alphabet.findIndex(a => a === digit.toUpperCase());
    if(dec < 0) {
      console.error(hex);
      throw new Error('Incorrect hex formatting');
    }
    return acc + dec * (16 ** idx);
  }, 0);
}

export function hexToRgba(hex, a) {
  const [r,g,b] = _.chunk(hex, 2).map(hexToDec);
  return `rgba(${r}, ${g}, ${b}, ${a})`;
}

export function writeText(str) {
  return new Promise(((resolve, reject) => {
    const range = document.createRange();
    range.selectNodeContents(document.body);
    document.getSelection().addRange(range);

    let success = false;
    function listener(e) {
      e.clipboardData.setData('text/plain', str);
      e.preventDefault();
      success = true;
    }
    document.addEventListener('copy', listener);
    document.execCommand('copy');
    document.removeEventListener('copy', listener);

    document.getSelection().removeAllRanges();

    success ? resolve() : reject();
  })).catch((error) => {
    console.error(error);
  });;
};

// trim patps to match dojo, chat-cli
export function cite(ship) {
  let patp = ship, shortened = '';
  if (patp.startsWith('~')) {
    patp = patp.substr(1);
  }
  // comet
  if (patp.length === 56) {
    shortened = '~' + patp.slice(0, 6) + '_' + patp.slice(50, 56);
    return shortened;
  }
  // moon
  if (patp.length === 27) {
    shortened = '~' + patp.slice(14, 20) + '^' + patp.slice(21, 27);
    return shortened;
  }
  return `~${patp}`;
}

export function alphabetiseAssociations(associations) {
  const result = {};
  Object.keys(associations).sort((a, b) => {
    let aName = a.substr(1);
    let bName = b.substr(1);
    if (associations[a].metadata && associations[a].metadata.title) {
      aName = associations[a].metadata.title !== ''
        ? associations[a].metadata.title
        : a.substr(1);
    }
    if (associations[b].metadata && associations[b].metadata.title) {
      bName = associations[b].metadata.title !== ''
        ? associations[b].metadata.title
        : b.substr(1);
    }
    return aName.toLowerCase().localeCompare(bName.toLowerCase());
  }).map((each) => {
    result[each] = associations[each];
  });
  return result;
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
          // TODO behavior for unicode doesn't match +wood's,
          //     but we can probably get away with that for now.
          add = '~' + charCode.toString(16) + '.';
        }
    }
    out = out + add;
  }
  return '~.' + out;
}

// used in Links

export function makeRoutePath(
  resource,
  popout = false,
  page = 0,
  url = null,
  index = 0,
  compage = 0
) {
  let route = "/~link" + (popout ? "/popout" : "") + resource;
  if (!url) {
    if (page !== 0) {
      route = route + "/" + page;
    }
  } else {
    route = `${route}/${page}/${index}/${base64urlEncode(url)}`;
    if (compage !== 0) {
      route = route + "/" + compage;
    }
  }
  return route;
}

export function amOwnerOfGroup(groupPath) {
  if (!groupPath) return false;
  const groupOwner = /(\/~)?\/~([a-z-]{3,})\/.*/.exec(groupPath)[2];
  return window.ship === groupOwner;
}

export function getContactDetails(contact) {
  const member = !contact;
  contact = contact || {
    nickname: "",
    avatar: null,
    color: "0x0",
  };
  const nickname = contact.nickname || "";
  const color = uxToHex(contact.color || "0x0");
  const avatar = contact.avatar || null;
  return { nickname, color, member, avatar };
}

export function stringToSymbol(str) {
  let result = '';
  for (var i = 0; i < str.length; i++) {
    var n = str.charCodeAt(i);
    if (((n >= 97) && (n <= 122)) ||
      ((n >= 48) && (n <= 57))) {
      result += str[i];
    } else if ((n >= 65) && (n <= 90)) {
      result += String.fromCharCode(n + 32);
    } else {
      result += '-';
    }
  }
  result = result.replace(/^[\-\d]+|\-+/g, '-');
  result = result.replace(/^\-+|\-+$/g, '');
  if (result === '') {
    return dateToDa(new Date());
  }
  return result;
}
