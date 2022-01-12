import {
  getVats,
  Vats,
  scryLag,
  getBlockers,
  Vat,
  kilnInstall,
  kilnPause,
  kilnResume
} from '@urbit/api';
import create from 'zustand';
import produce from 'immer';
import { useCallback } from 'react';
import api from './api';
import { fakeRequest, useMockData } from './util';
import { mockVats } from './mock-data';

interface KilnState {
  vats: Vats;
  loaded: boolean;
  fetchVats: () => Promise<void>;
  lag: boolean;
  fetchLag: () => Promise<void>;
  changeOTASource: (ship: string) => Promise<void>;
  toggleOTAs: (desk: string, on: boolean) => Promise<void>;
  set: (s: KilnState) => void;
}
const useKilnState = create<KilnState>((set, get) => ({
  vats: useMockData ? mockVats : {},
  lag: !!useMockData,
  loaded: false,
  fetchVats: async () => {
    if (useMockData) {
      await fakeRequest({}, 500);
      set({ loaded: true });
      return;
    }
    const vats = await api.scry<Vats>(getVats);
    set({ vats, loaded: true });
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
          if (!draft.vats.base.arak.rail) {
            return;
          }
          draft.vats.base.arak.rail.ship = ship;
        })
      );
      return;
    }

    await api.poke(kilnInstall(ship, 'kids', 'base'));
  },
  toggleOTAs: async (desk: string, on: boolean) => {
    set(
      produce((draft: KilnState) => {
        const { arak } = draft.vats[desk];
        if (!arak.rail) {
          return;
        }
        if (on) {
          arak.rail.paused = false;
        } else {
          arak.rail.paused = true;
        }
      })
    );

    await (useMockData ? fakeRequest('') : api.poke(on ? kilnResume(desk) : kilnPause(desk)));
    await get().fetchVats(); // refresh vat state
  },
  set: produce(set)
}));

api.subscribe({
  app: 'hood',
  path: '/kiln/vats',
  event: () => {
    useKilnState.getState().fetchVats();
  }
});

const selBlockers = (s: KilnState) => getBlockers(s.vats);
export function useBlockers() {
  return useKilnState(selBlockers);
}

export function useVat(desk: string): Vat | undefined {
  return useKilnState(useCallback((s) => s.vats[desk], [desk]));
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
