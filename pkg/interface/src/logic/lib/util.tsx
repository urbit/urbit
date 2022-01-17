import React, { useCallback, useEffect, useMemo, useState } from 'react';
import _ from 'lodash';
import { patp2dec } from 'urbit-ob';
import f  from 'lodash/fp';
import { Association, Contact, Patp } from '@urbit/api';
import { enableMapSet } from 'immer';
/* eslint-disable max-lines */
import anyAscii from 'any-ascii';
import { sigil as sigiljs, stringRenderer } from '@tlon/sigil-js';
import bigInt, { BigInteger } from 'big-integer';
import { IconRef, Workspace } from '~/types';
import { Text } from '@tlon/indigo-react';

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

export type GraphModule = 'link' | 'post' | 'chat' | 'publish';

export const getModuleIcon = (mod: GraphModule): IconRef => {
  if (mod === 'link') {
    return 'Collection';
  }

  if (mod === 'post') {
    return 'Dashboard';
  }

  return _.capitalize(mod) as IconRef;
};

export function wait(ms: number) {
  return new Promise((resolve, reject) => {
    setTimeout(resolve, ms);
  });
}

export function parentPath(path: string) {
  return _.dropRight(path.split('/'), 1).join('/');
}

/*
 * undefined -> initial
 * null -> disabled feed
 * string -> enabled feed
 */
export function getFeedPath(association: Association): string | null | undefined {
  const metadata = association?.metadata;
  if(!metadata) {
    return undefined;
  }
  if (metadata?.config && 'group' in metadata?.config && metadata.config?.group) {
    if ('resource' in metadata.config.group) {
      return metadata.config.group.resource;
    }
    return null;
  }
  return undefined;
}

export const getChord = (e: KeyboardEvent) => {
  const chord = [e.key];
  if(e.metaKey) {
    chord.unshift('meta');
  }
  if(e.ctrlKey) {
    chord.unshift('ctrl');
  }
  if(e.shiftKey) {
    chord.unshift('shift');
  }
  return chord.join('+');
};

export function getResourcePath(workspace: Workspace, path: string, joined: boolean, mod: string) {
  const base = workspace.type === 'group'
    ? `/~landscape${workspace.group}`
    : workspace.type === 'home'
    ? '/~landscape/home'
    : '/~landscape/messages';
  return `${base}/${joined ? 'resource' : 'join'}/${mod}${path}`;
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
  const timeSinceEpoch = bigInt(unix).multiply(DA_SECOND).divide(bigInt(1000));
  return DA_UNIX_EPOCH.add(timeSinceEpoch);
}

export function dmCounterparty(resource: string) {
  const [,,ship,name] = resource.split('/');
  return ship === `~${window.ship}` ? `~${name.slice(4)}` : ship;
}

export function isDm(resource: string) {
  const [,,,name] = resource.split('/');
  return name.startsWith('dm--');
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

/*
 *  Clamp a number between a min and max
 */
export function clamp(x: number, min: number, max: number) {
  return Math.max(min, Math.min(max, x));
}

/*
 * Euclidean modulo
 */
export function modulo(x: number, mod: number) {
  return x < 0 ? (x % mod + mod) % mod : x % mod;
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

export function deSig(ship: string): string {
  if (!ship) {
    return '';
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
    f.reverse,
    f.chunk(4),
    // eslint-disable-next-line prefer-arrow-callback
    f.map(x => _.dropWhile(x, function(y: unknown) {
      return y === '0';
    }).reverse().join('')),
    f.reverse,
    f.join('.')
  )(hex.split(''));
  return `0x${ux}`;
};

export function writeText(str: string | null): Promise<void> {
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

export const citeNickname = (ship: string, showNickname?: boolean, nickname?: string) =>
  showNickname && nickname ? `${nickname} - ${cite(ship)}` : cite(ship);

// trim patps to match dojo, chat-cli
export function cite(ship: string): string | Element {
  let patp = ship,
    shortened: string | Element = '';
  if (patp === null || patp === '') {
    return '';
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
    shortened = <>
      <Text mono fontSize="10px" verticalAlign="top">{patp.slice(0, 13)}</Text>
      {'~' + patp.slice(14, 27) + '^'}
    </>;
    return shortened;
  }
  return `~${patp}`;
}

export function stripNonWord(string: string): string {
  return string.replace(/[^\p{L}\p{N}\p{Z}]/gu, '');
}

export function alphabeticalOrder(a: string, b: string) {
  return stripNonWord(a).toLowerCase().trim().localeCompare(stripNonWord(b).toLowerCase().trim());
}

export function lengthOrder(a: string, b: string) {
  return b.length - a.length;
}

export const keys = <T extends {}>(o: T) => Object.keys(o) as (keyof T)[];

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
        //  eslint-disable-next-line
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
  const ascii = anyAscii(str);
  let result = '';
  for (let i = 0; i < ascii.length; i++) {
    const n = ascii.charCodeAt(i);
    if ((n >= 97 && n <= 122) || (n >= 48 && n <= 57)) {
      result += ascii[i];
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
/*
 * Formats a numbers as a `@ud` inserting dot where needed
 */
export function numToUd(num: number) {
  return f.flow(
    f.split(''),
    f.reverse,
    f.chunk(3),
    f.reverse,
    f.map(f.flow(f.reverse, f.join(''))),
    f.join('.')
  )(num.toString());
}

export function patpToUd(patp: Patp) {
  return numToUd(patp2dec(patp));
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
      // @ts-ignore  need better window typings
      window.onbeforeunload = undefined;
    };
  }, [shouldPreventDefault]);
}

export function pluralize(text: string, isPlural = false, vowel = false) {
  return isPlural ? `${text}s` : `${vowel ? 'an' : 'a'} ${text}`;
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
  const onMouseOver = useCallback(() => setHovering(true), []);
  const onMouseLeave = useCallback(() => setHovering(false), []);
  const bind = useMemo(() => ({
    onMouseOver,
    onMouseLeave
  }), [onMouseLeave, onMouseOver]);

  return useMemo(() => ({ hovering, bind }), [hovering, bind]);
};

export function withHovering<T>(Component: React.ComponentType<T>) {
  return React.forwardRef((props, ref) => {
    const { hovering, bind } = useHovering();
    // @ts-ignore needs type signature on return?
    return <Component ref={ref} hovering={hovering} bind={bind} {...props} />;
  });
}

const DM_REGEX = /ship\/~([a-z]|-)*\/dm--/;
export function getItemTitle(association: Association): string {
  if (DM_REGEX.test(association.resource)) {
    const [, , ship, name] = association.resource.split('/');
    if (deSig(ship) === window.ship) {
      return cite(`~${name.slice(4)}`);
    }
    return cite(ship);
  }
  return association.metadata.title ?? association.resource ?? '';
}

export const svgDataURL = svg => 'data:image/svg+xml;base64,' + btoa(svg);

export const svgBlobURL = svg => URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml' }));


export function binaryIndexOf(arr: BigInteger[], target: BigInteger): number | undefined {
  let leftBound = 0;
  let rightBound = arr.length - 1;
  while(leftBound <= rightBound) {
    const halfway = Math.floor((leftBound + rightBound) / 2);
    if(arr[halfway].greater(target)) {
      leftBound = halfway + 1;
    } else if (arr[halfway].lesser(target)) {
      rightBound = halfway - 1;
    } else {
      return halfway;
    }
  }
  return undefined;
}

export async function jsonFetch<T>(info: RequestInfo, init?: RequestInit): Promise<T> {
  const res = await fetch(info, init);
  if(!res.ok) {
    throw new Error('Bad Fetch Response');
  }
  const data = await res.json();
  return data as T;
}

export function clone<T>(a: T) {
  return JSON.parse(JSON.stringify(a)) as T;
}

export function toHarkPath(path: string, index = '') {
  return `/graph/${path.slice(6)}${index}`;
}

export function toHarkPlace(graph: string, index = '') {
  return {
    desk: (window as any).desk,
    path: toHarkPath(graph, index)
  };
}

export function createStorageKey(name: string): string {
  return `~${window.ship}/${window.desk}/${name}`;
}

// for purging storage with version updates
export function clearStorageMigration<T>() {
  return {} as T;
}

export const storageVersion = parseInt(process.env.LANDSCAPE_STORAGE_VERSION, 10);
