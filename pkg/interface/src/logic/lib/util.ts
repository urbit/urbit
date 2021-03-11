import { useEffect, useState } from 'react';
import _ from 'lodash';
import f, { compose, memoize } from 'lodash/fp';
import bigInt, { BigInteger } from 'big-integer';
import { Association, Contact } from '@urbit/api';
import useLocalState from '../state/local';
import produce, { enableMapSet } from 'immer';
import useSettingsState from '../state/settings';
import { State, UseStore } from 'zustand';
import { Cage } from '~/types/cage';
import { BaseState } from '../state/base';

enableMapSet();

export const MOBILE_BROWSER_REGEX = /Android|webOS|iPhone|iPad|iPod|BlackBerry/i;

export const MOMENT_CALENDAR_DATE = {
  sameDay: '[Today]',
  nextDay: '[Tomorrow]',
  nextWeek: 'dddd',
  lastDay: '[Yesterday]',
  lastWeek: '[Last] dddd',
  sameElse: '~YYYY.M.D'
};

export const getModuleIcon = (mod: string) => {
 if (mod === 'link') {
    return 'Collection';
  }
  return _.capitalize(mod);
};

export function wait(ms: number) {
  return new Promise((resolve, reject) => {
    setTimeout(resolve, ms);
  });
}

export function appIsGraph(app: string) {
  return app === 'publish' || app == 'link';
}

export function parentPath(path: string) {
  return _.dropRight(path.split('/'), 1).join('/');
}

const DA_UNIX_EPOCH = bigInt('170141184475152167957503069145530368000'); // `@ud` ~1970.1.1
const DA_SECOND = bigInt('18446744073709551616'); // `@ud` ~s1
export function daToUnix(da: BigInteger) {
  // ported from +time:enjs:format in hoon.hoon
  const offset = DA_SECOND.divide(bigInt(2000));
  const epochAdjusted = offset.add(da.subtract(DA_UNIX_EPOCH));

  return Math.round(
    epochAdjusted.multiply(bigInt(1000)).divide(DA_SECOND).toJSNumber()
  );
}

export function unixToDa(unix: number) {
  const timeSinceEpoch =  bigInt(unix).multiply(DA_SECOND).divide(bigInt(1000));
  return DA_UNIX_EPOCH.add(timeSinceEpoch);
}

export function makePatDa(patda: string) {
  return bigInt(udToDec(patda));
}

export function udToDec(ud: string): string {
  return ud.replace(/\./g, '');
}

export function decToUd(str: string): string {
  return _.trimStart(
    f.flow(
      f.split(''),
      f.reverse,
      f.chunk(3),
      f.map(f.flow(f.reverse, f.join(''))),
      f.reverse,
      f.join('.')
    )(str),
    '0.'
  );
}

/**
 *  Clamp a number between a min and max
 */
export function clamp(x: number, min: number, max: number) {
  return Math.max(min, Math.min(max, x));
}

// color is a #000000 color
export function adjustHex(color: string, amount: number): string {
  return f.flow(
    f.split(''),
    f.chunk(2), // get RGB channels
    f.map(c => parseInt(c.join(''), 16)), // as hex
    f.map(c => clamp(c + amount, 0, 255).toString(16)), // adjust
    f.join(''),
    res => `#${res}` // format
  )(color.slice(1));
}

export function resourceAsPath(resource: any) {
  const { name, ship } = resource;
  return `/ship/~${ship}/${name}`;
}

export function uuid() {
  let str = '0v';
  str += Math.ceil(Math.random() * 8) + '.';
  for (let i = 0; i < 5; i++) {
    let _str = Math.ceil(Math.random() * 10000000).toString(32);
    _str = ('00000' + _str).substr(-5, 5);
    str += _str + '.';
  }

  return str.slice(0, -1);
}

/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st: string) {
  const dub = function (n: string) {
    return parseInt(n) < 10 ? '0' + parseInt(n) : n.toString();
  };
  const da = st.split('..');
  const bigEnd = da[0].split('.');
  const lilEnd = da[1].split('.');
  const ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(
    lilEnd[0]
  )}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

/*
  Goes from:
    (javascript Date object)
  To:
    ~2018.7.17..23.15.09..5be5    // urbit @da
*/

export function dateToDa(d: Date, mil = false) {
  const fil = function (n: number) {
    return n >= 10 ? n : '0' + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${d.getUTCMonth() + 1}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? '..0000' : ''}`
  );
}

export function deSig(ship: string) {
  if (!ship) {
    return null;
  }
  return ship.replace('~', '');
}

export function uxToHex(ux: string) {
  if (ux.length > 2 && ux.substr(0, 2) === '0x') {
    const value = ux.substr(2).replace('.', '').padStart(6, '0');
    return value;
  }

  const value = ux.replace('.', '').padStart(6, '0');
  return value;
}

export const hexToUx = (hex) => {
  const ux = f.flow(
    f.chunk(4),
    f.map(x => _.dropWhile(x, y => y === 0).join('')),
    f.join('.')
  )(hex.split(''));
  return `0x${ux}`;
};

export function writeText(str: string) {
  return new Promise<void>((resolve, reject) => {
    const range = document.createRange();
    range.selectNodeContents(document.body);
    document?.getSelection()?.addRange(range);

    let success = false;
    function listener(e) {
      e.clipboardData.setData('text/plain', str);
      e.preventDefault();
      success = true;
    }
    document.addEventListener('copy', listener);
    document.execCommand('copy');
    document.removeEventListener('copy', listener);

    document?.getSelection()?.removeAllRanges();

    success ? resolve() : reject();
  }).catch((error) => {
    console.error(error);
  });
}

// trim patps to match dojo, chat-cli
export function cite(ship: string) {
  let patp = ship,
    shortened = '';
  if (patp === null || patp === '') {
    return null;
  }
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

export function alphabeticalOrder(a: string, b: string) {
  return a.toLowerCase().localeCompare(b.toLowerCase());
}

export function lengthOrder(a: string, b: string) {
  return b.length - a.length;
}

//  TODO: deprecated
export function alphabetiseAssociations(associations: any) {
  const result = {};
  Object.keys(associations)
    .sort((a, b) => {
      let aName = a.substr(1);
      let bName = b.substr(1);
      if (associations[a].metadata && associations[a].metadata.title) {
        aName =
          associations[a].metadata.title !== ''
            ? associations[a].metadata.title
            : a.substr(1);
      }
      if (associations[b].metadata && associations[b].metadata.title) {
        bName =
          associations[b].metadata.title !== ''
            ? associations[b].metadata.title
            : b.substr(1);
      }
      return alphabeticalOrder(aName, bName);
    })
    .map((each) => {
      result[each] = associations[each];
    });
  return result;
}

// encode the string into @ta-safe format, using logic from +wood.
// for example, 'some Chars!' becomes '~.some.~43.hars~21.'
//
export function stringToTa(str: string) {
  let out = '';
  for (let i = 0; i < str.length; i++) {
    const char = str[i];
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
        const charCode = str.charCodeAt(i);
        if (
          (charCode >= 97 && charCode <= 122) || // a-z
          (charCode >= 48 && charCode <= 57) || // 0-9
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

export function amOwnerOfGroup(groupPath: string) {
  if (!groupPath)
return false;
  const groupOwner = /(\/~)?\/~([a-z-]{3,})\/.*/.exec(groupPath)?.[2];
  return window.ship === groupOwner;
}

export function getContactDetails(contact: any) {
  const member = !contact;
  contact = contact || {
    nickname: '',
    avatar: null,
    color: '0x0'
  };
  const nickname = contact.nickname || '';
  const color = uxToHex(contact.color || '0x0');
  const avatar = contact.avatar || null;
  return { nickname, color, member, avatar };
}

export function stringToSymbol(str: string) {
  let result = '';
  for (let i = 0; i < str.length; i++) {
    const n = str.charCodeAt(i);
    if ((n >= 97 && n <= 122) || (n >= 48 && n <= 57)) {
      result += str[i];
    } else if (n >= 65 && n <= 90) {
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

/**
 * Formats a numbers as a `@ud` inserting dot where needed
 */
export function numToUd(num: number) {
  return f.flow(
    f.split(''),
    f.reverse,
    f.chunk(3),
    f.reverse,
    f.map(s => s.join('')),
    f.join('.')
  )(num.toString());
}

export function usePreventWindowUnload(shouldPreventDefault: boolean, message = 'You have unsaved changes. Are you sure you want to exit?') {
  useEffect(() => {
    if (!shouldPreventDefault)
return;
    const handleBeforeUnload = (event) => {
      event.preventDefault();
      return message;
    };
    window.addEventListener('beforeunload', handleBeforeUnload);
    window.onbeforeunload = handleBeforeUnload;
    return () => {
      window.removeEventListener('beforeunload', handleBeforeUnload);
      // @ts-ignore
      window.onbeforeunload = undefined;
    };
  }, [shouldPreventDefault]);
}

export function pluralize(text: string, isPlural = false, vowel = false) {
  return isPlural ? `${text}s`: `${vowel ? 'an' : 'a'} ${text}`;
}

// Hide is an optional second parameter for when this function is used in class components
export function useShowNickname(contact: Contact | null, hide?: boolean): boolean {
  const hideState = useSettingsState(state => state.calm.hideNicknames);
  const hideNicknames = typeof hide !== 'undefined' ? hide : hideState;
  return !!(contact && contact.nickname && !hideNicknames);
}

interface useHoveringInterface {
  hovering: boolean;
  bind: {
    onMouseOver: () => void,
    onMouseLeave: () => void
  }
}

export const useHovering = (): useHoveringInterface => {
  const [hovering, setHovering] = useState(false);
  const bind = {
    onMouseOver: () => setHovering(true),
    onMouseLeave: () => setHovering(false)
  };
  return { hovering, bind };
};

const DM_REGEX = /ship\/~([a-z]|-)*\/dm--/;
export function getItemTitle(association: Association) {
  if(DM_REGEX.test(association.resource)) {
    const [,,ship,name] = association.resource.split('/');
    if(ship.slice(1) === window.ship) {
      return cite(`~${name.slice(4)}`);
    }
    return cite(ship);
  }
  return association.metadata.title || association.resource;
}

