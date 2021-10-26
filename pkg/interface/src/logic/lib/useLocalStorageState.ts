import { useMemo, useEffect, useState } from 'react';

export function retrieve<T>(key: string, initial: T): T {
  const s = localStorage.getItem(key);
  if (s) {
    try {
      return JSON.parse(s) as T;
    } catch (e) {
      return initial;
    }
  }
  return initial;
}

export function useLocalStorageState<T>(key: string, initial: T): any {
  const [state, setState] = useState(() => retrieve(key, initial));

  useEffect(() => {
    setState(retrieve(key, initial));
  }, [key]);

  useEffect(() => {
    localStorage.setItem(key, JSON.stringify(state));
  }, [state]);

  return useMemo(() => [state, setState] as const, [state, setState]);
}
