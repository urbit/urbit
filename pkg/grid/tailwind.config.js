const colors = require('tailwindcss/colors');

module.exports = {
  mode: 'jit',
  purge: ['./index.html', './src/**/*.{js,ts,jsx,tsx}'],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      colors: {
        transparent: "transparent",
        white: "#FFFFFF",
        black: "#000000",
        gray: {
          ...colors.trueGray,
          100: "#F2F2F2",
          200: "#CCCCCC",
          300: "#B3B3B3",
          400: "#808080",
          500: "#666666",
        },
        blue: {
          100: "#E9F5FF",
          200: "#D3EBFF",
          300: "#BCE2FF",
          400: "#219DFF",
        },
        red: {
          100: "#FFF6F5",
          200: "#FFC6C3",
          400: "#FF4136",
        },
        green: {
          100: "#E6F5F0",
          200: "#B3E2D1",
          400: "#009F65",
        },
        yellow: {
          100: "#FFF9E6",
          200: "#FFEEB3",
          300: "#FFDD66",
          400: "#FFC700",
        },
      },
      fontFamily: {
        sans: [
          '"Inter"',
          '"Inter UI"',
          "-apple-system",
          "BlinkMacSystemFont",
          '"San Francisco"',
          '"Helvetica Neue"',
          "Arial",
          "sans-serif",
        ],
        mono: [
          '"Source Code Pro"',
          '"Roboto mono"',
          '"Courier New"',
          "monospace",
        ],
      },
      minWidth: theme => theme('spacing'),
    },
  },
  variants: {
    extend: {
      opacity: ['hover-none']
    },
  },
  plugins: [
    require('@tailwindcss/aspect-ratio'),
    require('tailwindcss-touch')()
  ],
}
