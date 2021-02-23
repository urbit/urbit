import { useEffect, useState } from 'react';
import _ from 'lodash';
import f from 'lodash/fp';
import produce from 'immer';

import { Association, Contact, cite, dateToDa } from '@urbit/api';

import useLocalState from '~/logic/state/local';

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

export function parentPath(path: string) {
  return _.dropRight(path.split('/'), 1).join('/');
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

export function usePreventWindowUnload(shouldPreventDefault: boolean, message = 'You have unsaved changes. Are you sure you want to exit?') {
  useEffect(() => {
    if (!shouldPreventDefault) return;
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
  const hideNicknames = typeof hide !== 'undefined' ? hide : useLocalState(state => state.hideNicknames);
  return Boolean(contact && contact.nickname && !hideNicknames);
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

export const stateSetter = <StateType>(fn: (state: StateType) => void, set): void => {
  // TODO this is a stub for the store debugging
  return set(produce(fn));
};