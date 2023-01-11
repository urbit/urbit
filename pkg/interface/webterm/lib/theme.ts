import { ITheme } from 'xterm';

export const makeTheme = (dark: boolean): ITheme => {
  let fg, bg: string;
  if (dark) {
    fg = 'white';
    bg = 'rgb(26,26,26)';
  } else {
    fg = 'black';
    bg = 'white';
  }
  // TODO  indigo colors.
  //      we can't pluck these from ThemeContext because they have transparency.
  //      technically xterm supports transparency, but it degrades performance.
  return {
    foreground: fg,
    background: bg,
    brightBlack: '#7f7f7f',  // NOTE  slogs
    cursor: fg,
    cursorAccent: bg,
    selection: fg
  };
};
