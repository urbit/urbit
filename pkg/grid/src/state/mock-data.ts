import _ from 'lodash-es';
import { Allies, Charges, DocketHrefGlob, Treaties, Treaty } from '@urbit/api/docket';
import systemUrl from '../assets/system.png';
import { SystemNotification } from './hark-types';

export const appMetaData: Pick<Treaty, 'cass' | 'hash' | 'website' | 'license' | 'version'> = {
  cass: '~2021.8.11..05.11.10..b721',
  hash: '0v6.nj6ls.l7unh.l9bhk.d839n.n8nlq.m2dmc.fj80i.pvqun.uhg6g.1kk0h',
  website: 'https://tlon.io',
  license: 'MIT',
  version: '2.0.1'
};

const makeHref = (base: string): DocketHrefGlob => ({ glob: { base } });

export const mockTreaties: Treaties = {
  '~zod/groups': {
    ship: '~zod',
    desk: 'groups',
    title: 'Groups',
    info: 'Simple Software for Community Assembly',
    href: makeHref('groups'),
    color: '#CDE7EF',
    ...appMetaData
  },
  '~zod/messages': {
    title: 'Messages',
    ship: '~zod',
    desk: 'messages',
    href: makeHref('messaages'),
    info: 'A lengthier description of the app down here',
    color: '#8BE789',
    ...appMetaData
  },
  '~zod/calls': {
    title: 'Calls',
    ship: '~zod',
    desk: 'calls',
    href: makeHref('calls'),
    info: 'A lengthier description of the app down here',
    color: '#C2D6BE',
    ...appMetaData
  },
  '~zod/bitcoin-wallet': {
    title: 'Bitcoin Wallet',
    ship: '~zod',
    desk: 'bitcoin-wallet',
    href: makeHref('bitcoin-wallet'),
    info: 'A lengthier description of the app down here',
    color: '#F0AE70',
    ...appMetaData
  },
  '~zod/system': {
    title: 'System',
    ship: '~zod',
    desk: 'system',
    href: makeHref('system'),
    info: 'A lengthier description of the app down here',
    color: '#2D0118',
    image: systemUrl,
    ...appMetaData
  },
  '~zod/my-apps': {
    title: 'My Apps',
    ship: '~zod',
    desk: 'my-apps',
    href: makeHref('my-apps'),
    info: 'A lengthier description of the app down here',
    color: '#D8B14E',
    ...appMetaData
  },
  '~zod/go': {
    title: 'Go',
    ship: '~zod',
    desk: 'go',
    href: makeHref('go'),
    info: 'A lengthier description of the app down here',
    color: '#A58E52',
    ...appMetaData
  },
  '~zod/terminal': {
    title: 'Terminal',
    ship: '~zod',
    desk: 'terminal',
    href: makeHref('terminal'),
    info: 'A lengthier description of the app down here',
    color: '#2D382B',
    ...appMetaData
  },
  '~zod/pomodoro': {
    title: 'Pomodoro',
    ship: '~zod',
    desk: 'pomodoro',
    href: makeHref('pomodoro'),
    info: 'A lengthier description of the app down here',
    color: '#EE5432',
    ...appMetaData
  },
  '~zod/clocks': {
    title: 'Clocks',
    ship: '~zod',
    desk: 'clocks',
    href: makeHref('clocks'),
    info: 'A lengthier description of the app down here',
    color: '#DCDCDC',
    ...appMetaData
  },
  '~zod/uniswap': {
    title: 'Uniswap',
    ship: '~zod',
    desk: 'uniswap',
    href: makeHref('uniswap'),
    info: 'A lengthier description of the app down here',
    color: '#FDA1FF',
    ...appMetaData
  },
  '~zod/inbox': {
    title: 'Inbox',
    ship: '~zod',
    desk: 'inbox',
    href: makeHref('inbox'),
    color: '#FEFFBA',
    ...appMetaData
  }
};

export const mockCharges: Charges = _.reduce(
  mockTreaties,
  (acc, val, key) => {
    const [, desk] = key.split('/');
    const chad = { glob: null };
    if (desk === 'inbox') {
      return acc;
    }

    return { ...acc, [desk]: { ...val, chad } };
  },
  {} as Charges
);

const charter = Object.keys(mockTreaties);

export const mockAllies: Allies = [
  '~zod',
  '~nocsyx-lassul',
  '~nachus-hollyn',
  '~nalbel_litzod',
  '~litmus^ritten',
  '~nalput_litzod',
  '~nalrex_bannus',
  '~nalrys'
].reduce((acc, val) => ({ ...acc, [val]: charter }), {});

export const mockBlockedChargeNotification: SystemNotification = {
  type: 'system-updates-blocked',
  charges: ['~zod/groups', '~zod/pomodoro']
};
