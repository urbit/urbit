import systemUrl from '../assets/system.png';
import goUrl from '../assets/go.png';
import { Provider, Treaties, Treaty } from './docket-types';

export const providers: Provider[] = [
  {
    shipName: '~zod',
    nickname: 'Tlon Corporation'
  },
  {
    shipName: '~nocsyx-lassul',
    status: 'technomancing an electron wrapper for urbit'
  },
  {
    shipName: '~nachus-hollyn'
  },
  {
    shipName: '~nalbel_litzod',
    status: 'congratulations'
  },
  {
    shipName: '~litmus^ritten'
  },
  {
    shipName: '~nalput_litzod',
    status: 'Queen'
  },
  {
    shipName: '~nalrex_bannus',
    status: 'Script, command and inspect your Urbit. Use TUI applications'
  },
  {
    shipName: '~nalrys',
    status: 'hosting coming soon'
  }
];

export const appMetaData: Pick<Treaty, 'cass' | 'hash' | 'website' | 'license' | 'version'> = {
  cass: '~2021.8.11..05.11.10..b721',
  hash: '0v6.nj6ls.l7unh.l9bhk.d839n.n8nlq.m2dmc.fj80i.pvqun.uhg6g.1kk0h',
  website: 'https://tlon.io',
  license: 'MIT',
  version: '2.0.1'
};

export const treaties: Treaties = {
  groups: {
    ship: '~zod',
    desk: 'groups',
    title: 'Groups',
    info: 'Simple Software for Community Assembly',
    status: 'active',
    base: 'groups',
    color: '##CDE7EF',
    ...appMetaData
  },
  messages: {
    title: 'Messages',
    ship: '~zod',
    desk: 'messages',
    status: 'active',
    base: 'messages',
    info: 'A lengthier description of the app down here',
    color: '##8BE789',
    ...appMetaData
  },
  calls: {
    title: 'Calls',
    ship: '~zod',
    desk: 'calls',
    status: 'active',
    base: 'calls',
    info: 'A lengthier description of the app down here',
    color: '##C2D6BE',
    ...appMetaData
  },
  'bitcoin-wallet': {
    title: 'Bitcoin Wallet',
    ship: '~zod',
    desk: 'bitcoin-wallet',
    status: 'active',
    base: 'bitcoin-wallet',
    info: 'A lengthier description of the app down here',
    color: '##F0AE70',
    ...appMetaData
  },
  system: {
    title: 'System',
    ship: '~zod',
    desk: 'system',
    status: 'active',
    base: 'system',
    info: 'A lengthier description of the app down here',
    color: '##2D0118',
    img: systemUrl,
    ...appMetaData
  },
  'my-apps': {
    title: 'My Apps',
    ship: '~zod',
    desk: 'groups',
    status: 'active',
    base: 'my-apps',
    info: 'A lengthier description of the app down here',
    color: '##D8B14E',
    ...appMetaData
  },
  go: {
    title: 'Go',
    ship: '~zod',
    desk: 'go',
    status: 'active',
    base: 'go',
    info: 'A lengthier description of the app down here',
    color: '##A58E52',
    img: goUrl,
    ...appMetaData
  },
  terminal: {
    title: 'Terminal',
    ship: '~zod',
    desk: 'terminal',
    status: 'active',
    base: 'terminal',
    info: 'A lengthier description of the app down here',
    color: '##2D382B',
    ...appMetaData
  },
  pomodoro: {
    title: 'Pomodoro',
    ship: '~zod',
    desk: 'pomodoro',
    status: 'active',
    base: 'pomodoro',
    info: 'A lengthier description of the app down here',
    color: '##EE5432',
    ...appMetaData
  },
  clocks: {
    title: 'Clocks',
    ship: '~zod',
    desk: 'clocks',
    status: 'active',
    base: 'clocks',
    info: 'A lengthier description of the app down here',
    color: '##DCDCDC',
    ...appMetaData
  },
  uniswap: {
    title: 'Uniswap',
    ship: '~zod',
    desk: 'uniswap',
    status: 'active',
    base: 'uniswap',
    info: 'A lengthier description of the app down here',
    color: '##FDA1FF',
    ...appMetaData
  },
  inbox: {
    title: 'Inbox',
    ship: '~zod',
    desk: 'inbox',
    status: 'active',
    base: 'inbox',
    color: '##FEFFBA',
    ...appMetaData
  }
};
