import { useEffect, useState } from 'react';
import { useTheme } from './settings';
import useTermState from './state';

export function useDark() {
  const [osDark, setOsDark] = useState(false);

  useEffect(() => {
    const themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    const update = (e: MediaQueryListEvent) => {
      setOsDark(e.matches);
    };
    setOsDark(themeWatcher.matches);
    themeWatcher.addListener(update);

    return () => {
      themeWatcher.removeListener(update);
    }

  }, []);

  const theme = useTermState(s => s.theme);
  return theme === 'dark' || (osDark && theme === 'auto');
}
