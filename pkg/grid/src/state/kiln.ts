import { getVats, Vats, scryLag, getBlockers, Vat } from '@urbit/api';
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
  set: (s: KilnState) => void;
}
const useKilnState = create<KilnState>((set) => ({
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
