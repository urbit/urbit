module.exports = {
  presets: ['@babel/preset-env', '@babel/typescript', '@babel/preset-react'],
  plugins: [
    '@babel/transform-runtime',
    '@babel/plugin-proposal-object-rest-spread',
    '@babel/plugin-proposal-optional-chaining',
    '@babel/plugin-proposal-class-properties',
    'react-hot-loader/babel'
  ]
};
