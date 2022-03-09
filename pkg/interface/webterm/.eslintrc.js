module.exports = exports = {
  "rules": {
    "spaced-comment": 0,
  },
  "extends": [
    "eslint:recommended",
    "plugin:import/errors",
    "plugin:react/recommended",
  ],
  "settings": {
    "react": {
      "version": "detect"
    },
    "import/resolver": {
      typescript: {} // this loads <rootdir>/tsconfig.json to eslint
    },
  },
  "env": {
    "browser": true,
    "es6": true
  },
  "plugins": ["import", "react-hooks"]
}
