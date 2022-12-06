import {
  Allies,
  Charges,
  DocketHrefGlob,
  Treaties,
  Treaty,
  Notification,
  HarkContent,
  HarkBody,
  unixToDa,
  Contact,
  Contacts,
  Timebox,
  harkBinToId,
  Pikes
} from '@urbit/api';
import _ from 'lodash';
import systemUrl from '../assets/system.png';

export const appMetaData: Pick<Treaty, 'cass' | 'hash' | 'website' | 'license' | 'version'> = {
  cass: {
    da: '~2021.9.13..05.41.04..ae65',
    ud: 1
  },
  hash: '0v6.nj6ls.l7unh.l9bhk.d839n.n8nlq.m2dmc.fj80i.pvqun.uhg6g.1kk0h',
  website: 'https://tlon.io',
  license: 'MIT',
  version: '2.0.1'
};

const makeHref = (base: string): DocketHrefGlob => ({ glob: { base } });

export const mockTreaties: Treaties = {
  '~zod/garden': {
    ship: '~zod',
    desk: 'garden',
    title: 'System',
    info: 'Your Urbit Home',
    href: makeHref('garden'),
    color: '#E2C050',
    ...appMetaData
  },
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
    href: makeHref('messages'),
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

    if (desk === 'calls') {
      return { ...acc, [desk]: { ...val, chad: { hung: 'glob failed' } } };
    }

    if (desk === 'messages') {
      return { ...acc, [desk]: { ...val, chad: { install: null } } };
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

function ship(s: string) {
  return { ship: s };
}

function text(t: string) {
  return { text: t };
}

function createDmNotification(...content: HarkContent[]): HarkBody {
  return {
    title: [ship('~hastuc-dibtux'), text(' messaged you')],
    time: unixToDa(Date.now() - 3_600).toJSNumber(),
    content,
    binned: '/',
    link: '/'
  };
}

function createBitcoinNotif(amount: string) {
  return {
    title: [ship('~silnem'), text(` sent you ${amount}`)],
    time: unixToDa(Date.now() - 3_600).toJSNumber(),
    content: [],
    binned: '/',
    link: '/'
  };
}

function createGroupNotif(to: string): HarkBody {
  return {
    title: [ship('~ridlur-figbud'), text(` invited you to ${to}`)],
    content: [],
    time: unixToDa(Date.now() - 3_600).toJSNumber(),
    binned: '/',
    link: '/'
  };
}

window.desk = window.desk || 'garden';

function createMockSysNotification(path: string, body: HarkBody[] = []) {
  return {
    bin: {
      place: {
        desk: window.desk,
        path
      },
      path: '/'
    },
    time: Date.now() - 3_600,
    body
  };
}

const lag = createMockSysNotification('/lag');
const blocked = {
  bin: {
    place: {
      desk: window.desk,
      path: '/desk/base'
    },
    path: '/blocked'
  },
  time: Date.now() - 3_600,
  body: []
};
const onboard = createMockSysNotification('/onboard');

const updateNotification = createMockSysNotification('/desk/bitcoin', [
  {
    title: [{ text: 'App "Bitcoin" updated to version 1.0.1' }],
    time: 0,
    content: [],
    link: '/desk/bitcoin',
    binned: '/'
  }
]);

export function createMockNotification(desk: string, body: HarkBody[]): Notification {
  return {
    bin: {
      place: {
        desk,
        path: '/'
      },
      path: '/'
    },
    time: Date.now() - 3_600,
    body
  };
}

export const mockNotifications: Timebox = _.keyBy(
  [
    lag,
    blocked,
    onboard,
    updateNotification,
    createMockNotification('groups', [
      createDmNotification(text('ie the hook agent responsible for marking the notifications')),
      createDmNotification(ship('~hastuc-dibtux'), text(' sent a link'))
    ]),
    createMockNotification('bitcoin-wallet', [createBitcoinNotif('0.025 BTC')]),
    createMockNotification('groups', [createGroupNotif('a Group: Tlon Corporation')])
  ],
  (not) => harkBinToId(not.bin)
);

const contact: Contact = {
  nickname: '',
  bio: '',
  status: '',
  color: '#000000',
  avatar: null,
  cover: null,
  groups: [],
  'last-updated': 0
};

export const mockContacts: Contacts = {
  '~zod': {
    ...contact,
    nickname: 'Tlon Corporation'
  },
  '~nocsyx-lassul': {
    ...contact,
    status: 'technomancing an electron wrapper for urbit',
    color: '#4c00ff'
  },
  '~nachus-hollyn': {
    ...contact,
    avatar: 'https://i.pinimg.com/originals/20/62/59/2062590a440f717a2ae1065ad8e8a4c7.gif'
  },
  '~nalbel_litzod': {
    ...contact,
    nickname: 'Queen',
    color: '#0a1b0a'
  },
  '~litmus^ritten': {
    ...contact
  },
  '~nalput_litzod': {
    ...contact
  },
  '~nalrex_bannus': {
    ...contact,
    status: 'Script, command and inspect your Urbit. Use TUI applications'
  },
  '~nalrys': {
    ...contact,
    status: 'hosting coming soon',
    color: '#130c06'
  }
};

export const mockPikes: Pikes = {
  kids: {
    sync: null,
    zest: 'dead',
    wefts: [],
    hash: '0v19.q7u27.omps3.fbhf4.53rai.co157.pben7.pu94n.63v4p.3kcb7.iafj0'
  },
  garden: {
    sync: {
      desk: 'garden',
      ship: '~mister-dister-dozzod-dozzod'
    },
    zest: 'live',
    wefts: [],
    hash: '0v18.hbbs6.onu15.skjkv.qrfgl.vf4oo.0igo5.2q0d3.6r3r8.2dkmo.oa04m'
  },
  landscape: {
    sync: {
      desk: 'landscape',
      ship: '~lander-dister-dozzod-dozzod'
    },
    zest: 'live',
    wefts: [],
    hash: '0v1t.qln8k.cskmt.cn6lv.gu335.jfba6.kte90.iqqn3.aj67b.t389a.8imuo'
  },
  base: {
    sync: null,
    zest: 'live',
    wefts: [],
    hash: '0v1e.b5auh.6u82i.hqk1r.22kli.4ubef.a1cbo.3g532.6l49k.g0i8e.t6eid'
  }
};
