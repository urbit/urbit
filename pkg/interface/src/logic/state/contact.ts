import { Contact, deSig, Patp, Rolodex, uxToHex } from '@urbit/api';
import { useCallback } from 'react';
import _ from 'lodash';
import { reduce, reduceNacks } from '../reducers/contact-update';
import {
  createState,
  createSubscription,
  reduceStateN
} from './base';
import { sigil as sigiljs, stringRenderer } from '@tlon/sigil-js';
import { foregroundFromBackground } from '~/logic/lib/sigil';

export interface ContactState {
  contacts: Rolodex;
  isContactPublic: boolean;
  nackedContacts: Set<Patp>;
}

// @ts-ignore investigate zustand types
const useContactState = createState<ContactState>(
  'Contact',
  {
    contacts: {},
    nackedContacts: new Set(),
    isContactPublic: false
  },
  ['nackedContacts'],
  [
    (set, get) =>
      createSubscription('contact-pull-hook', '/nacks', (e) => {
        const data = e?.resource;
        if (data) {
          reduceStateN(get(), data, [reduceNacks]);
        }
      }),
    (set, get) =>
      createSubscription('contact-store', '/all', (e) => {
        const data = _.get(e, 'contact-update', false);
        if (data) {
          reduceStateN(get(), data, reduce);
        }
      })
  ]
);

export function useContact(ship: string) {
  return useContactState(
    useCallback(s => s.contacts[`~${deSig(ship)}`] as Contact | null, [ship])
  );
}

export function useOurContact() {
  return useContact(`~${window.ship}`);
}

export const favicon = () => {
  let background = '#ffffff';
  const contacts = useContactState.getState().contacts;
  if (Object.prototype.hasOwnProperty.call(contacts, `~${window.ship}`)) {
    background = `#${uxToHex(contacts[`~${window.ship}`].color)}`;
  }
  const foreground = foregroundFromBackground(background);
  const svg = sigiljs({
    patp: window.ship,
    renderer: stringRenderer,
    size: 16,
    colors: [background, foreground]
  });
  return svg;
};

export default useContactState;
