import { scryLag, kilnInstall, kilnPause, kilnResume, getPikes, Pikes, Pike } from '@urbit/api';
import create from 'zustand';
import produce from 'immer';
import { useCallback } from 'react';
import api from './api';
import { fakeRequest, useMockData } from './util';
import { mockPikes } from './mock-data';

interface KilnState {
  pikes: Pikes;
  loaded: boolean;
  lag: boolean;
  fetchLag: () => Promise<void>;
  fetchPikes: () => Promise<void>;
  changeOTASource: (ship: string) => Promise<void>;
  toggleOTAs: (desk: string, on: boolean) => Promise<void>;
  set: (s: KilnState) => void;
  initializeKiln: () => Promise<void>;
}
const useKilnState = create<KilnState>((set, get) => ({
  pikes: useMockData ? mockPikes : {},
  lag: !!useMockData,
  loaded: false,
  fetchPikes: async () => {
    if (useMockData) {
      await fakeRequest({}, 500);
      set({ loaded: true });
      return;
    }
    const pikes = await api.scry<Pikes>(getPikes);
    set({ pikes, loaded: true });
  },
  fetchLag: async () => {
    const lag = await api.scry<boolean>(scryLag);
    set({ lag });
  },
  changeOTASource: async (ship: string) => {
    if (useMockData) {
      await fakeRequest('');
      set(
        produce((draft: KilnState) => {
          if (!draft.pikes.base.sync?.ship) {
            return;
          }
          draft.pikes.base.sync.ship = ship;
        })
      );
      return;
    }

    await api.poke(kilnInstall(ship, 'kids', 'base'));
  },
  toggleOTAs: async (desk: string, on: boolean) => {
    set(
      produce((draft: KilnState) => {
        const pike = draft.pikes[desk];
        if (!pike) {
          return;
        }

        pike.zest = on ? 'live' : 'held';
      })
    );

    await (useMockData ? fakeRequest('') : api.poke(on ? kilnResume(desk) : kilnPause(desk)));
    await get().fetchPikes(); // refresh pikes state
  },
  set: produce(set),
  initializeKiln: async () => {
    await get().fetchLag();
    await get().fetchPikes();
  }
}));

const selPikes = (s: KilnState) => s.pikes;
export function usePikes(): Pikes {
  return useKilnState(selPikes);
}

export function usePike(desk: string): Pike | undefined {
  return useKilnState(useCallback((s) => s.pikes[desk], [desk]));
}

const selLag = (s: KilnState) => s.lag;
export function useLag() {
  return useKilnState(selLag);
}

const selLoaded = (s: KilnState) => s.loaded;
export function useKilnLoaded() {
  return useKilnState(selLoaded);
}

export default useKilnState;
