const defaultTheme = require('tailwindcss/defaultTheme');
const resolveConfig = require('tailwindcss/resolveConfig');
const { Theme, ThemeManager } = require('tailwindcss-theming/api');

const themableProperties = [
  'spacing',
  'fontFamily',
  //'fontSize', would require change in tailwindcss-theming
  'fontWeight',
  'letterSpacing',
  'lineHeight',
  'borderRadius',
  'borderWidth',
  'boxShadow'
];

function variablizeTheme(themeConfig, theme) {
  themableProperties.forEach((prop) => {
    const propSet = themeConfig[prop];
    Object.entries(propSet).forEach(([key, value]) => {
      theme.setVariable(key, value, prop, prop);
    });
  });
}

const config = resolveConfig({
  theme: {
    fontFamily: {
      sans: [
        'Inter',
        'Inter UI',
        '-apple-system',
        'BlinkMacSystemFont',
        'San Francisco',
        'Helvetica Neue',
        'Arial',
        'sans-serif'
      ],
      mono: ['Source Code Pro', 'Roboto mono', 'Courier New', 'monospace']
    },
    extend: {
      lineHeight: {
        tight: 1.2,
        snug: 1.33334,
        relaxed: 1.66667
      }
    }
  }
});

const base = new Theme().addColors({
  transparent: 'transparent',
  white: '#FFFFFF',
  black: '#000000',
  gray: {
    50: '#F2F2F2',
    100: '#E5E5E5',
    200: '#CCCCCC',
    300: '#B3B3B3',
    400: '#999999',
    500: '#808080',
    600: '#666666',
    700: '#4D4D4D',
    800: '#333333',
    900: '#1A1A1A'
  },
  blue: {
    50: '#EFF9FF',
    100: '#C8EDFF',
    200: '#A0E1FF',
    300: '#5FBFFF',
    400: '#219DFF',
    500: '#0F75D8',
    600: '#0252B2',
    700: '#00388B',
    800: '#002364',
    900: '#00133E'
  },
  red: {
    50: '#FFF4F2',
    100: '#FFDED6',
    200: '#FFC8B9',
    300: '#FC9B84',
    400: '#F57456',
    500: '#EE5432',
    600: '#D03B22',
    700: '#B12918',
    800: '#931C13',
    900: '#751410'
  },
  orange: {
    50: '#FFF4EF',
    100: '#FFE2CE',
    200: '#FFCEAB',
    300: '#FFA56F',
    400: '#FF7E36',
    500: '#D85E1E',
    600: '#B2420C',
    700: '#8B2B00',
    800: '#641E00',
    900: '#3E1100'
  },
  green: {
    100: '#E6F5F0',
    200: '#B3E2D1',
    300: '#009F65'
  },
  yellow: {
    100: '#FFF9E6',
    200: '#FFEEB3',
    300: '#FFDD66',
    400: '#FFC700'
  }
});
variablizeTheme(config.theme, base);

const dark = new Theme()
  .setName('dark')
  .targetable()
  .addColors({
    transparent: 'transparent',
    white: '#000000',
    black: '#FFFFFF',
    gray: {
      50: '#1A1A1A',
      100: '#333333',
      200: '#4D4D4D',
      300: '#666666',
      400: '#808080',
      500: '#999999',
      600: '#B3B3B3',
      700: '#CCCCCC',
      800: '#E5E5E5',
      900: '#F2F2F2'
    },
    red: {
      50: '#751410',
      100: '#931C13',
      200: '#B12918',
      300: '#D03B22',
      400: '#EE5432',
      500: '#F57456',
      600: '#FC9B84',
      700: '#FFC8B9',
      800: '#FFDED6',
      900: '#FFF4F2'
    },
    blue: {
      50: '#00133E',
      100: '#002364',
      200: '#00388B',
      300: '#0252B2',
      400: '#0F75D8',
      500: '#219DFF',
      600: '#5FBFFF',
      700: '#A0E1FF',
      800: '#C8EDFF',
      900: '#EFF9FF'
    },
    orange: {
      50: '#3E1100',
      100: '#641E00',
      200: '#8B2B00',
      300: '#B2420C',
      400: '#D85E1E',
      500: '#FF7E36',
      600: '#FFA56F',
      700: '#FFCEAB',
      800: '#FFE2CE',
      900: '#FFF4EF'
    },
    green: {
      100: '#182722',
      200: '#134231',
      300: '#009F65'
    },
    yellow: {
      100: '#312B18',
      200: '#5F4E13',
      300: '#A4820B',
      400: '#FFC700'
    }
  });

const themes = new ThemeManager().setDefaultTheme(base).addTheme(dark);

module.exports = {
  mode: 'jit',
  purge: ['./index.html', './src/**/*.{js,ts,jsx,tsx}'],
  darkMode: 'class', // or 'media' or 'class'
  theme: {
    extend: {
      minWidth: (theme) => theme('spacing')
    }
  },
  screens: {
    ...defaultTheme.screens,
    xl: '1440px',
    '2xl': '2200px'
  },
  variants: {
    extend: {
      opacity: ['hover-none']
    }
  },
  plugins: [
    require('@tailwindcss/aspect-ratio'),
    require('tailwindcss-touch')(),
    require('tailwindcss-theming')({
      themes,
      strategy: 'class'
    })
  ]
};
