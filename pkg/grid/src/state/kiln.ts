import { getVats, Vats } from '@urbit/api';
import create from 'zustand';
import produce from 'immer';
import api from './api';

interface KilnState {
  vats: Vats;
  fetchVats: () => Promise<void>;
  set: (s: KilnState) => void;
}
export const useKilnState = create<KilnState>((set) => ({
  vats: {},
  fetchVats: async () => {
    const vats = await api.scry<Vats>(getVats);
    set ({ vats });
  },
  set: produce(set)
}));

export default useKilnState;
