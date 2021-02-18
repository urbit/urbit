import { useState, useCallback, useEffect } from 'react';

function retrieve<T>(key: string, initial: T): T {
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

interface SetStateFunc<T> {
  (t: T): T;
}
type SetState<T> = T | SetStateFunc<T>;
export function useLocalStorageState<T>(key: string, initial: T) {
  const [state, _setState] = useState(() => retrieve(key, initial));

  useEffect(() => {
    _setState(retrieve(key, initial));
  }, [key]);

  const setState = useCallback(
    (s: SetState<T>) => {
      const updated = typeof s === 'function' ? s(state) : s;
      _setState(updated);
      localStorage.setItem(key, JSON.stringify(updated));
    },
    [_setState, key, state]
  );

  return [state, setState] as const;
}
