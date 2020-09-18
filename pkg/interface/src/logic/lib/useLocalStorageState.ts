import { useState, useCallback } from 'react';

export function useLocalStorageState<T>(key: string, initial: T) {
  const [state, _setState] = useState(() => {
    const s = localStorage.getItem(key);
    if(s) {
      return JSON.parse(s) as T;
    }
    return initial;

  });

  const setState = useCallback((s: T) => {
    _setState(s);
    localStorage.setItem(key, JSON.stringify(s));

  }, [_setState]);

  return [state, setState] as const;
}


