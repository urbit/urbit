import { getVats, Vats, scryLag, KilnDiff } from '@urbit/api';
import create from 'zustand';
import produce from 'immer';
import api from './api';
import {getBlockers} from '../../../npm/api/hood';

interface KilnState {
  vats: Vats;
  fetchVats: () => Promise<void>;
  lag: boolean;
  fetchLag: () => Promise<void>;
  set: (s: KilnState) => void;
}
export const useKilnState = create<KilnState>((set) => ({
  vats: {},
  lag: true,
  fetchVats: async () => {
    const vats = await api.scry<Vats>(getVats);
    console.log(vats);
    set ({ vats });
  },
  fetchLag: async () => {
    const lag = await api.scry<boolean>(scryLag);
    set({ lag });
  },
  set: produce(set)
}));

const selBlockers = (s: KilnState) => getBlockers(s.vats)
export function useBlockers() {
  return useKilnState(selBlockers);
}

const selLag = (s: KilnState) => s.lag;
export function useLag() {
  return useKilnState(selLag);
}


window.kiln = useKilnState;

export default useKilnState;
