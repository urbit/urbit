import { hsla, parseToHsla } from 'color2k';
import { useCurrentTheme } from '../state/local';

function getDarkColor(color: string): string {
  const hslaColor = parseToHsla(color);
  return hsla(hslaColor[0], hslaColor[1], 1 - hslaColor[2], 1);
}

export const useTileColor = (color: string) => {
  const theme = useCurrentTheme();

  return {
    theme,
    tileColor: theme === 'dark' ? getDarkColor(color) : color
  };
};
