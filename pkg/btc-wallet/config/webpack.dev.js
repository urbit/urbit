const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');
const fs = require('fs-extra');
const _ = require('lodash');

function copy(src,dest) {
  return new Promise((res,rej) =>
    fs.copy(src,dest, err => err ? rej(err) : res()));
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
  publicPath: '/apps/bitcoin/',
};

const router = _.mapKeys(urbitrc.FLEET || {}, (value, key) => `${key}.localhost:9000`);

if(urbitrc.URL) {
  devServer = {
    ...devServer,
    index: 'index.html',
    proxy: [{
      target: 'http://localhost:9000',
      changeOrigin: true,
      target: urbitrc.URL,
      router,
      context: path => {
        if(path === '/apps/bitcoin/desk.js') {
          return true;
        }
        return !path.startsWith('/apps/bitcoin')
      }
    }]
  };
}

module.exports = {
  node: { fs: 'empty' },
  mode: 'development',
  entry: {
    app: './src/index.js'
  },
  module: {
    rules: [
      {
        test: /\.(j|t)sx?$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env', ['@babel/preset-react', {
              runtime: 'automatic',
              development: 'true',
              importSource: '@welldone-software/why-did-you-render',
            }]],
            plugins: [
              '@babel/transform-runtime',
              '@babel/plugin-proposal-object-rest-spread',
              '@babel/plugin-proposal-optional-chaining',
              '@babel/plugin-proposal-class-properties',
              'react-hot-loader/babel'
            ]
          }
        },
        exclude: /node_modules/
      },
      {
        test: /\.css$/i,
        use: [
          // Creates `style` nodes from JS strings
          'style-loader',
          // Translates CSS into CommonJS
          'css-loader',
          // Compiles Sass to CSS
          'sass-loader'
        ]
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx']
  },
  devtool: 'inline-source-map',
  devServer: devServer,
  plugins: [
    new UrbitShipPlugin(urbitrc),
    new HtmlWebpackPlugin({
      title: 'Bitcoin Wallet',
      template: './public/index.html'
    })

  ],
  watch: true,
  watchOptions: {
    poll: true,
    ignored: '/node_modules/'
  },
  output: {
    filename: 'index.js',
    chunkFilename: 'index.js',
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/apps/bitcoin/',
    globalObject: 'this'
  },
  optimization: {
    minimize: false,
    usedExports: true
  }
};
