const HtmlWebpackPlugin = require('html-webpack-plugin');
const urbitrc = require('./urbitrc');
const webpack = require('webpack');
const { config } = require('@urbit/webpack-conf');

const baseConfig = config('bitcoin', urbitrc);

const finalConfig = {
  ...baseConfig,
  resolve: {
    ...baseConfig.resolve,
    fallback: {
      ...baseConfig.resolve.fallback,
      Buffer: require.resolve('buffer')
    }
  },
  plugins: [
    new HtmlWebpackPlugin({
      title: 'Bitcoin',
      template: './public/index.html'
    }),
    new webpack.ProvidePlugin({
      process: 'process/browser',
      Buffer: ['buffer', 'Buffer']
    })
  ]
};

module.exports = finalConfig;
