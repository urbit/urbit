import _ from 'lodash';

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
      console.log(hex);
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
