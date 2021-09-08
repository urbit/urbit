/* eslint-disable no-param-reassign */
import create from 'zustand';
import { Notification, harkBinEq, makePatDa, readAll, decToUd, unixToDa } from '@urbit/api';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
/* eslint-disable-next-line camelcase */
import { unstable_batchedUpdates } from 'react-dom';
import produce from 'immer';
import { map } from 'lodash';
import api from './api';
import { useMockData } from './util';
import { mockNotifications } from './mock-data';

interface HarkStore {
  unreads: Notification[];
  reads: BigIntOrderedMap<Notification[]>;
  set: (f: (s: HarkStore) => void) => void;
  readAll: () => Promise<void>;
  getMore: () => Promise<void>;
}

export const useHarkStore = create<HarkStore>((set, get) => ({
  unreads: useMockData ? mockNotifications : [],
  reads: new BigIntOrderedMap<Notification[]>(),
  set: (f) => {
    const newState = produce(get(), f);
    set(newState);
  },
  readAll: async () => {
    const { set: innerSet } = get();
    innerSet((state) => {
      state.unreads.forEach((note) => {
        const time = makePatDa(note.time as any);
        const box = state.reads.get(time) || [];
        state.reads.set(time, [...box, note]);
      });
      state.unreads = [];
    });
    await api.poke(readAll);
  },
  getMore: async () => {
    const { reads } = get();
    const idx = decToUd((reads.peekSmallest()?.[0] || unixToDa(Date.now() * 1000)).toString());
    const update = await api.scry({
      app: 'hark-store',
      path: `/recent/inbox/${idx}/5`
    });
    reduceHark(update);
  }
}));

function reduceHark(u: any) {
  const { set } = useHarkStore.getState();
  if (!u) {
    return;
  }
  if ('more' in u) {
    u.more.forEach((upd: any) => {
      reduceHark(upd);
    });
  } else if ('all-stats' in u) {
    // TODO: probably ignore?
  } else if ('added' in u) {
    set((state) => {
      state.unreads = state.unreads.filter((unread) => !harkBinEq(unread.bin, u.added.bin));

      state.unreads.push(u.added);
    });
  } else if ('timebox' in u) {
    const { timebox } = u;
    const notifications = map(timebox.notifications, 'notification');
    if (timebox.time) {
      console.log(notifications);
      set((state) => {
        state.reads = state.reads.set(makePatDa(timebox.time), notifications);
      });
    } else {
      set((state) => {
        state.unreads = [...state.unreads, ...notifications];
      });
    }
  }
}

api.subscribe({
  app: 'hark-store',
  path: '/updates',
  event: (u: any) => {
    /* eslint-ignore-next-line camelcase */
    unstable_batchedUpdates(() => {
      reduceHark(u);
    });
  }
});
