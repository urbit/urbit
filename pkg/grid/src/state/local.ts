import create from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';

interface LocalState {
  protocolHandling: boolean;
  currentTheme: 'light' | 'dark';
  set: (f: (s: LocalState) => void) => void;
}

export const useLocalState = create<LocalState>(
  persist(
    (set, get) => ({
      set: (f) => set(produce(get(), f)),
      currentTheme: 'light',
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

const selCurrentTheme = (s: LocalState) => s.currentTheme;
export function useCurrentTheme() {
  return useLocalState(selCurrentTheme);
}

export const setLocalState = (f: (s: LocalState) => void) => useLocalState.getState().set(f);
