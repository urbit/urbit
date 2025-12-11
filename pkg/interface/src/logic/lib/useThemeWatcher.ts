import dark from '@tlon/indigo-dark';
import light from '@tlon/indigo-light';
import { useEffect } from 'react';
import useLocalState, { selectLocalState } from '../state/local';
import useSettingsState, { selectDisplayState } from '../state/settings';

const selLocal = selectLocalState(['dark', 'set']);

export function useThemeWatcher() {
  const { set, dark: isDark } = useLocalState(selLocal);
  const display = useSettingsState(selectDisplayState);
  const theme = ((isDark && display.theme == 'auto') || display.theme == 'dark') ? dark : light;

  useEffect(() => {
    const updateTheme = (e: MediaQueryListEvent) => set(s => ({ dark: e.matches }));
    const updateMobile = (e: MediaQueryListEvent) => set(s => ({ mobile: e.matches }));
    const updateSmall = (e: MediaQueryListEvent) => set(s => ({ breaks: { sm: e.matches } }));
    const updateMedium = (e: MediaQueryListEvent) => set(s => ({ breaks: { md: e.matches } }));
    const updateLarge = (e: MediaQueryListEvent) => set(s => ({ breaks: { lg: e.matches } }));

    const themeWatcher = window.matchMedia('(prefers-color-scheme: dark)');
    const mobileWatcher = window.matchMedia(`(max-width: ${theme.breakpoints[0]})`);
    const smallWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[0]})`);
    const mediumWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[1]})`);
    const largeWatcher = window.matchMedia(`(min-width: ${theme.breakpoints[2]})`);

    themeWatcher.addEventListener('change', updateTheme);
    mobileWatcher.addEventListener('change', updateMobile);
    smallWatcher.addEventListener('change', updateSmall);
    mediumWatcher.addEventListener('change', updateMedium);
    largeWatcher.addEventListener('change', updateLarge);

    updateTheme({ matches: themeWatcher.matches } as MediaQueryListEvent);
    updateMobile({ matches: mobileWatcher.matches } as MediaQueryListEvent);
    updateSmall({ matches: smallWatcher.matches } as MediaQueryListEvent);
    updateMedium({ matches: mediumWatcher.matches } as MediaQueryListEvent);
    updateLarge({ matches: largeWatcher.matches } as MediaQueryListEvent);

    return () => {
      themeWatcher.removeEventListener('change', updateTheme);
      mobileWatcher.removeEventListener('change', updateMobile);
      smallWatcher.removeEventListener('change', updateSmall);
      mediumWatcher.removeEventListener('change', updateMedium);
      largeWatcher.removeEventListener('change', updateLarge);
    };
  }, []);

  return {
    display,
    theme
  };
}
