import { hsla, parseToHsla } from 'color2k';
import { usePreferencesStore } from '../nav/preferences/usePreferencesStore';

function getDarkColor(color: string): string {
  const hslaColor = parseToHsla(color);
  return hsla(hslaColor[0], hslaColor[1], 1 - hslaColor[2], 1);
}

export const useTileColor = (color: string) => {
  const theme = usePreferencesStore((s) => s.currentTheme);

  return {
    theme,
    tileColor: theme === 'dark' ? getDarkColor(color) : color
  };
};
