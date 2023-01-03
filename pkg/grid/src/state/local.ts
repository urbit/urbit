import create from 'zustand';
import { persist } from 'zustand/middleware';
import produce from 'immer';
import { clearStorageMigration, createStorageKey, storageVersion } from './util';

interface LocalState {
  browserId: string;
  currentTheme: 'light' | 'dark';
  set: (f: (s: LocalState) => void) => void;
}

export const useLocalState = create<LocalState>(
  persist(
    (set, get) => ({
      set: (f) => set(produce(get(), f)),
      currentTheme: 'light',
      browserId: ''
    }),
    {
      name: createStorageKey('local'),
      version: storageVersion,
      migrate: clearStorageMigration
    }
  )
);

const selBrowserId = (s: LocalState) => s.browserId;
export function useBrowserId() {
  return useLocalState(selBrowserId);
}

const selCurrentTheme = (s: LocalState) => s.currentTheme;
export function useCurrentTheme() {
  return useLocalState(selCurrentTheme);
}

export const setLocalState = (f: (s: LocalState) => void) => useLocalState.getState().set(f);
