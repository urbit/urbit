import { darken, hsla, lighten, parseToHsla, readableColorIsBlack } from 'color2k';
import { useCurrentTheme } from '../state/local';
import { getDarkColor } from '../state/util';

function bgAdjustedColor(color: string, darkBg: boolean): string {
  return darkBg ? lighten(color, 0.1) : darken(color, 0.1);
}

function getMenuColor(color: string, darkBg: boolean): string {
  const hslaColor = parseToHsla(color);
  const satAdjustedColor = hsla(hslaColor[0], Math.max(0.2, hslaColor[1]), hslaColor[2], 1);

  return bgAdjustedColor(satAdjustedColor, darkBg);
}

// makes tiles look broken because they blend into BG
function disallowWhiteTiles(color: string): string {
  const hslaColor = parseToHsla(color);
  return hslaColor[2] >= 0.95 ? darken(color, hslaColor[2] - 0.95) : color;
}

export const useTileColor = (color: string) => {
  const theme = useCurrentTheme();
  const darkTheme = theme === 'dark';
  const allowedColor = disallowWhiteTiles(color);
  const tileColor = darkTheme ? getDarkColor(allowedColor) : allowedColor;
  const darkBg = !readableColorIsBlack(tileColor);
  const lightText = darkBg !== darkTheme; // if not same, light text
  const suspendColor = darkTheme ? 'rgb(26,26,26)' : 'rgb(220,220,220)';

  return {
    theme,
    tileColor,
    menuColor: getMenuColor(tileColor, darkBg),
    suspendColor,
    suspendMenuColor: bgAdjustedColor(suspendColor, darkTheme),
    lightText
  };
};
