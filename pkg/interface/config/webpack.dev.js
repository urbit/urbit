const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');
const _ = require('lodash');
const { execSync } = require('child_process');

const GIT_DESC = execSync('git describe --always', { encoding: 'utf8' }).trim();

let devServer = {
  contentBase: path.join(__dirname, '../dist'),
  hot: true,
  port: 9000,
  host: '0.0.0.0',
  disableHostCheck: true,
  historyApiFallback: true,
  publicPath: '/apps/landscape/'
};

const router =  _.mapKeys(urbitrc.FLEET || {}, (value, key) => `${key}.localhost:9000`);

if(urbitrc.URL) {
  devServer = {
    ...devServer,
    index: 'index.html',
    proxy: [{
      changeOrigin: true,
      target: urbitrc.URL,
      router,
      context: (path) => {
        if(path === '/apps/landscape/desk.js') {
          return true;
        }
        return !path.startsWith('/apps/landscape');
      }
    }]
  };
}

module.exports = {
  mode: 'development',
  entry: {
    app: './src/index.js'
    // serviceworker: './src/serviceworker.js'
  },
  module: {
    rules: [
      {
        test: /\.(j|t)sx?$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env', '@babel/typescript', ['@babel/preset-react', {
              runtime: 'automatic',
              development: true,
              importSource: '@welldone-software/why-did-you-render'
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
        exclude: /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light|@tlon\/indigo-react)\/).*/
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
    new webpack.DefinePlugin({
      'process.env.LANDSCAPE_SHORTHASH': JSON.stringify(GIT_DESC),
      'process.env.TUTORIAL_HOST': JSON.stringify('~difmex-passed'),
      'process.env.TUTORIAL_GROUP': JSON.stringify('beginner-island'),
      'process.env.TUTORIAL_CHAT': JSON.stringify('introduce-yourself-7010'),
      'process.env.TUTORIAL_BOOK': JSON.stringify('guides-9684'),
      'process.env.TUTORIAL_LINKS': JSON.stringify('community-articles-2143')
    }),

    // new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: 'Landscape',
      template: './public/index.html'
    })
  ],
  watch: true,
  output: {
    filename: (pathData) => {
      return pathData.chunk.name === 'app' ? 'index.js' : '[name].js';
    },
    chunkFilename: '[name].js',
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/apps/landscape/',
    globalObject: 'this'
  },
  optimization: {
    minimize: false,
    usedExports: true
  }
};
