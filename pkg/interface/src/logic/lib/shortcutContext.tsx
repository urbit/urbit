import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import _ from 'lodash';

type Handler = (e: KeyboardEvent) => void;
const fallback: ShortcutContextProps = {
  add: () => {},
  remove: () => {},
};
const getChord = (e: KeyboardEvent) => {
  let chord = [e.key];
  if(e.metaKey) {
    chord.unshift('meta');
  }
  if(e.ctrlKey) {
    chord.unshift('ctrl');
  }
  return chord.join('+');
}

export const ShortcutContext = createContext(fallback);
export interface ShortcutContextProps {
  add: (cb: (e: KeyboardEvent) => void, key: string) => void;
  remove: (cb: (e: KeyboardEvent) => void, key: string) => void;
}
export function ShortcutContextProvider({ children }) {
  const [shortcuts, setShortcuts] = useState({} as Record<string, Handler>);
  const handlerRef = useRef<Handler>(() => {});

  const add = useCallback((cb: Handler, key: string) => {
    setShortcuts((s) => ({ ...s, [key]: cb }));
  }, []);
  const remove = useCallback((cb: Handler, key: string) => {
    setShortcuts((s) => (key in s ? _.omit(s, key) : s));
  }, []);

  useEffect(() => {
    function onKeypress(e: KeyboardEvent) {
      handlerRef.current(e);
    }
    document.addEventListener('keypress', onKeypress);
    return () => {
      document.removeEventListener('keypress', onKeypress);
    };
  }, []);

  useEffect(() => {
    handlerRef.current = function (e: KeyboardEvent) {
      const chord = getChord(e);
      console.log(chord);
      shortcuts?.[chord]?.(e);
    };
  }, [shortcuts]);

  return (
    <ShortcutContext.Provider value={{ add, remove }}>
      {children}
    </ShortcutContext.Provider>
  );
}

export function useShortcut(key: string, cb: Handler) {
  const { add, remove } = useContext(ShortcutContext);
  useEffect(() => {
    add(cb, key);
    return () => {
      remove(cb, key);
    };
  }, [key, cb]);
}
