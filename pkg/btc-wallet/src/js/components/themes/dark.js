import baseStyled from "styled-components";

const base = {
  white: "rgba(255,255,255,1)",
  black: "rgba(0,0,0,1)",
  red: "rgba(255,65,54,1)",
  yellow: "rgba(255,199,0,1)",
  green: "rgba(0,159,101,1)",
  blue: "rgba(0,142,255,1)",
};

const scales = {
  white05: "rgba(255,255,255,0.05)",
  white10: "rgba(255,255,255,0.1)",
  white20: "rgba(255,255,255,0.2)",
  white30: "rgba(255,255,255,0.3)",
  white40: "rgba(255,255,255,0.4)",
  white50: "rgba(255,255,255,0.5)",
  white60: "rgba(255,255,255,0.6)",
  white70: "rgba(255,255,255,0.7)",
  white80: "rgba(255,255,255,0.8)",
  white90: "rgba(255,255,255,0.9)",
  white100: "rgba(255,255,255,1)",
  black05: "rgba(0,0,0,0.05)",
  black10: "rgba(0,0,0,0.1)",
  black20: "rgba(0,0,0,0.2)",
  black30: "rgba(0,0,0,0.3)",
  black40: "rgba(0,0,0,0.4)",
  black50: "rgba(0,0,0,0.5)",
  black60: "rgba(0,0,0,0.6)",
  black70: "rgba(0,0,0,0.7)",
  black80: "rgba(0,0,0,0.8)",
  black90: "rgba(0,0,0,0.9)",
  black100: "rgba(0,0,0,1)",
  red05: "rgba(255,65,54,0.05)",
  red10: "rgba(255,65,54,0.1)",
  red20: "rgba(255,65,54,0.2)",
  red30: "rgba(255,65,54,0.3)",
  red40: "rgba(255,65,54,0.4)",
  red50: "rgba(255,65,54,0.5)",
  red60: "rgba(255,65,54,0.6)",
  red70: "rgba(255,65,54,0.7)",
  red80: "rgba(255,65,54,0.8)",
  red90: "rgba(255,65,54,0.9)",
  red100: "rgba(255,65,54,1)",
  yellow05: "rgba(255,199,0,0.05)",
  yellow10: "rgba(255,199,0,0.1)",
  yellow20: "rgba(255,199,0,0.2)",
  yellow30: "rgba(255,199,0,0.3)",
  yellow40: "rgba(255,199,0,0.4)",
  yellow50: "rgba(255,199,0,0.5)",
  yellow60: "rgba(255,199,0,0.6)",
  yellow70: "rgba(255,199,0,0.7)",
  yellow80: "rgba(255,199,0,0.8)",
  yellow90: "rgba(255,199,0,0.9)",
  yellow100: "rgba(255,199,0,1)",
  green05: "rgba(0,159,101,0.05)",
  green10: "rgba(0,159,101,0.1)",
  green20: "rgba(0,159,101,0.2)",
  green30: "rgba(0,159,101,0.3)",
  green40: "rgba(0,159,101,0.4)",
  green50: "rgba(0,159,101,0.5)",
  green60: "rgba(0,159,101,0.6)",
  green70: "rgba(0,159,101,0.7)",
  green80: "rgba(0,159,101,0.8)",
  green90: "rgba(0,159,101,0.9)",
  green100: "rgba(0,159,101,1)",
  blue05: "rgba(0,142,255,0.05)",
  blue10: "rgba(0,142,255,0.1)",
  blue20: "rgba(0,142,255,0.2)",
  blue30: "rgba(0,142,255,0.3)",
  blue40: "rgba(0,142,255,0.4)",
  blue50: "rgba(0,142,255,0.5)",
  blue60: "rgba(0,142,255,0.6)",
  blue70: "rgba(0,142,255,0.7)",
  blue80: "rgba(0,142,255,0.8)",
  blue90: "rgba(0,142,255,0.9)",
  blue100: "rgba(0,142,255,1)",
};

const util = {
  cyan: "#00FFFF",
  magenta: "#FF00FF",
  yellow: "#FFFF00",
  black: "#000000",
  gray0: "#333333"
};

const theme = {
  colors: {
    white: util.gray0,
    black: base.white,

    gray: scales.white60,
    lightGray: scales.white30,
    washedGray: scales.white05,

    red: base.red,
    lightRed: scales.red30,
    washedRed: scales.red05,

    yellow: base.yellow,
    lightYellow: scales.yellow30,
    washedYellow: scales.yellow10,

    green: base.green,
    lightGreen: scales.green30,
    washedGreen: scales.green10,

    blue: base.blue,
    lightBlue: scales.blue30,
    washedBlue: scales.blue10,

    none: "rgba(0,0,0,0)",

    scales: scales,
    util: util,
  },
  fonts: {
    sans: `"Inter", "Inter UI", -apple-system, BlinkMacSystemFont, 'San Francisco', 'Helvetica Neue', Arial, sans-serif`,
    mono: `"Source Code Pro", "Roboto mono", "Courier New", monospace`,
  },
  // font-size
  fontSizes: [
    12, // 0
    16, // 1
    24, // 2
    32, // 3
    48, // 4
    64, // 5
  ],
  // 	font-weight
  fontWeights: {
    thin: 300,
    regular: 400,
    bold: 600,
  },
  // line-height
  lineHeights: {
    min: 1.2,
    short: 1.333333,
    regular: 1.5,
    tall: 1.666666,
  },
  // border, border-top, border-right, border-bottom, border-left
  borders: ["none", "1px solid"],
  // margin, margin-top, margin-right, margin-bottom, margin-left, padding, padding-top, padding-right, padding-bottom, padding-left, grid-gap, grid-column-gap, grid-row-gap
  space: [
    0, // 0
    4, // 1
    8, // 2
    16, // 3
    24, // 4
    32, // 5
    48, // 6
    64, // 7
    96, // 8
  ],
  // border-radius
  radii: [
    0, // 0
    2, // 1
    4, // 2
    8, // 3
  ],
  // width, height, min-width, max-width, min-height, max-height
  sizes: [
    0, // 0
    4, // 1
    8, // 2
    16, // 3
    24, // 4
    32, // 5
    48, // 6
    64, // 7
    96, // 8
  ],
  // z-index
  zIndices: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  breakpoints: ["550px", "750px", "960px"],
};
export const styled = baseStyled;
export default theme;
