const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const MomentLocalesPlugin = require('moment-locales-webpack-plugin');
const webpack = require('webpack');
const { execSync } = require('child_process');
const urbitrc = require('./urbitrc');
const { config } = require('@urbit/webpack-conf');
const isDev = process.env.NODE_ENV !== 'production';

const GIT_DESC = execSync('git describe --always', { encoding: 'utf8' }).trim();

const plugins = isDev
  ? []
  : [new MomentLocalesPlugin(), new CleanWebpackPlugin()];

module.exports = {
  ...config('landscape', urbitrc),
  plugins: [
    new webpack.DefinePlugin({
      'process.env.LANDSCAPE_STREAM': JSON.stringify(
        process.env.LANDSCAPE_STREAM
      ),
      'process.env.LANDSCAPE_SHORTHASH': JSON.stringify(GIT_DESC),
      'process.env.TUTORIAL_HOST': JSON.stringify('~difmex-passed'),
      'process.env.TUTORIAL_GROUP': JSON.stringify('beginner-island'),
      'process.env.TUTORIAL_CHAT': JSON.stringify('introduce-yourself-7010'),
      'process.env.TUTORIAL_BOOK': JSON.stringify('guides-9684'),
      'process.env.TUTORIAL_LINKS': JSON.stringify('community-articles-2143')
    }),
    new HtmlWebpackPlugin({
      title: 'Landscape',
      template: './public/index.html'
    }),
    ...plugins
  ]
};
