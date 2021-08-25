import { getVats, Vats, scryLag, getBlockers } from '@urbit/api';
import create from 'zustand';
import produce from 'immer';
import { useCallback } from 'react';
import api from './api';
import { fakeRequest, useMockData } from './util';
import { mockVats } from './mock-data';

interface KilnState {
  vats: Vats;
  fetchVats: () => Promise<void>;
  lag: boolean;
  fetchLag: () => Promise<void>;
  set: (s: KilnState) => void;
}
export const useKilnState = create<KilnState>((set) => ({
  vats: useMockData ? mockVats : {},
  lag: !!useMockData,
  fetchVats: async () => {
    if (useMockData) {
      await fakeRequest({}, 500);
      return;
    }
    const vats = await api.scry<Vats>(getVats);
    set({ vats });
  },
  fetchLag: async () => {
    const lag = await api.scry<boolean>(scryLag);
    set({ lag });
  },
  set: produce(set)
}));
console.log(useKilnState.getState());

const selBlockers = (s: KilnState) => getBlockers(s.vats);
export function useBlockers() {
  return useKilnState(selBlockers);
}

export function useVat(desk: string) {
  return useKilnState(useCallback((s) => s.vats[desk], [desk]));
}

const selLag = (s: KilnState) => s.lag;
export function useLag() {
  return useKilnState(selLag);
}

export default useKilnState;
