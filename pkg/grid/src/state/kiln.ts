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
  vats: {},
  lag: false,
  loaded: false,
  fetchVats: async () => {
    const vats = await api.scry<Vats>(getVats);
    set({ vats, loaded: true });
  },
  fetchLag: async () => {
    const lag = await api.scry<boolean>(scryLag);
    set({ lag });
  },
  changeOTASource: async (ship: string) => {
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

    await api.poke(on ? kilnResume(desk) : kilnPause(desk));
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
