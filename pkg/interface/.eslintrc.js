module.exports = {
  extends: '@urbit',
  env: {
    'jest': true
  },
  rules: {
    // Because we use styled system, and use
    // the convention of each prop on a new line
    // we probably shouldn't keep this on
    'max-lines-per-function': ['off', {}]
  }
};
