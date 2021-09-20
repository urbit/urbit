import { darken, hsla, lighten, parseToHsla, readableColorIsBlack } from 'color2k';
import { useCurrentTheme } from '../state/local';

function getDarkColor(color: string): string {
  const hslaColor = parseToHsla(color);
  return hsla(hslaColor[0], hslaColor[1], 1 - hslaColor[2], 1);
}

function bgAdjustedColor(color: string, darkBg: boolean): string {
  return darkBg ? lighten(color, 0.1) : darken(color, 0.1);
}

function getMenuColor(color: string, darkBg: boolean): string {
  const hslaColor = parseToHsla(color);
  const satAdjustedColor = hsla(hslaColor[0], Math.max(0.2, hslaColor[1]), hslaColor[2], 1);

  return bgAdjustedColor(satAdjustedColor, darkBg);
}

export const useTileColor = (color: string) => {
  const theme = useCurrentTheme();
  const darkTheme = theme === 'dark';
  const tileColor = darkTheme ? getDarkColor(color) : color;
  const darkBg = !readableColorIsBlack(tileColor);
  const lightText = darkBg !== darkTheme; // if not same, light text
  const suspendColor = darkTheme ? 'rgb(26,26,26)' : 'rgb(220,220,220)';

  return {
    theme,
    tileColor: theme === 'dark' ? getDarkColor(color) : color,
    menuColor: getMenuColor(tileColor, darkBg),
    suspendColor,
    suspendMenuColor: bgAdjustedColor(suspendColor, darkBg),
    lightText
  };
};
