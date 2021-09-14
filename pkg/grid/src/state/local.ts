import create from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';

interface LocalState {
  protocolHandling: boolean;
  set: (f: (s: LocalState) => void) => void;
}

export const useLocalState = create<LocalState>(
  persist(
    (set, get) => ({
      set: (f) => set(produce(get(), f)),
      protocolHandling: false
    }),
    {
      name: 'grid-local'
    }
  )
);

const selProtocolHandling = (s: LocalState) => s.protocolHandling;
export function useProtocolHandling() {
  return useLocalState(selProtocolHandling);
}

export const setLocalState = (f: (s: LocalState) => void) => useLocalState.getState().set(f);
