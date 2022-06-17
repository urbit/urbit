const path = require('path');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
// const urbitrc = require('./urbitrc');

module.exports = {
  node: { fs: 'empty' },
  mode: 'production',
  entry: {
    app: './src/index.tsx',
  },
  module: {
    rules: [
      {
        test: /\.(j|t)sx?$/,
        use: {
          loader: 'ts-loader',
        },
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: [
          // Creates `style` nodes from JS strings
          'style-loader',
          // Translates CSS into CommonJS
          'css-loader',
        ],
      },
    ],
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx'],
  },
  devtool: 'source-map',
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: 'Bitcoin Wallet',
      template: './public/index.html',
    }),
  ],
  output: {
    filename: (pathData) => {
      return pathData.chunk.name === 'app'
        ? 'index.[contenthash].js'
        : '[name].js';
    },
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/apps/bitcoin/',
  },
  optimization: {
    minimize: true,
    usedExports: true,
  },
};
