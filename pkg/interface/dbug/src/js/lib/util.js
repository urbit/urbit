import _ from 'lodash';
import classnames from 'classnames';

export function makeRoutePath(resource, includeQuery = false) {
  let query = window.location.href.split('?')[1];
  if (includeQuery && query) {
    query = '?' + query;
  } else {
    query = '';
  }
  return '/~debug/' + resource + query;
}

export function msToDa(ms, mil) {
  const d = new Date(ms);
  const fil = function(n) {
    return n >= 10 ? n : "0" + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${(d.getUTCMonth() + 1)}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    //NOTE  poor man's pretty @da milliseconds
    `${!mil ? '' :
       '..' + Math.floor((d.getUTCMilliseconds() / 1000) * 0x10000)
              .toString(16).padStart(4, '0')}`
  );
}

export function renderDuct(duct) {
  return duct.reduce((a, b) => a + b + ' ', '');
}

// encode the string into @ta-safe format, using logic from +wood.
// for example, 'some Chars!' becomes '~.some.~43.hars~21.'
// this is equivalent to (scot %t string)
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
  return '~~' + out;
}
