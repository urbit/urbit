const path = require('path');
const webpack = require('webpack');
// const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');
const fs = require('fs-extra');
const _ = require('lodash');

function copy(src, dest) {
  return new Promise((res, rej) =>
    fs.copy(src, dest, (err) => (err ? rej(err) : res()))
  );
}

class UrbitShipPlugin {
  constructor(urbitrc) {
    this.piers = urbitrc.URBIT_PIERS;
  }

  apply(compiler) {
    compiler.hooks.afterEmit.tapPromise(
      'UrbitShipPlugin',
      async (compilation) => {
        const src = path.resolve(compiler.options.output.path, 'index.js');
      }
    );
  }
}

let devServer = {
  contentBase: path.join(__dirname, '../dist'),
  hot: true,
  port: 9000,
  host: '0.0.0.0',
  disableHostCheck: true,
  historyApiFallback: true,
};

const router = _.mapKeys(
  urbitrc.FLEET || {},
  (value, key) => `${key}.localhost:9000`
);

if (urbitrc.URL) {
  devServer = {
    ...devServer,
    index: '',
    proxy: {
      '/~btc/js/bundle/index.*.js': {
        target: 'http://localhost:9000',
        pathRewrite: (req, path) => {
          return '/index.js';
        },
      },
      '**': {
        changeOrigin: true,
        target: urbitrc.URL,
        router,
        // ensure proxy doesn't timeout channels
        proxyTimeout: 0,
      },
    },
  };
}

module.exports = {
  node: { fs: 'empty' },
  mode: 'development',
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
          // Compiles Sass to CSS
          'sass-loader',
        ],
      },
    ],
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx'],
  },
  devtool: 'inline-source-map',
  devServer: devServer,
  plugins: [new UrbitShipPlugin(urbitrc)],
  watch: true,
  watchOptions: {
    poll: true,
    ignored: '/node_modules/',
  },
  output: {
    filename: 'index.js',
    chunkFilename: 'index.js',
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/',
    globalObject: 'this',
  },
  optimization: {
    minimize: false,
    usedExports: true,
  },
};
