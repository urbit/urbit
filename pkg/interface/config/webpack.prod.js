const path = require('path');
// const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const MomentLocalesPlugin = require('moment-locales-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  mode: 'production',
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
            presets: ['@babel/preset-env', '@babel/typescript', '@babel/preset-react'],
            plugins: [
              'lodash',
              '@babel/transform-runtime',
              '@babel/plugin-proposal-object-rest-spread',
              '@babel/plugin-proposal-optional-chaining',
              '@babel/plugin-proposal-class-properties'
            ]
          }
        },
        exclude: /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light)\/).*/
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
  devtool: 'source-map',
  // devServer: {
  //   contentBase: path.join(__dirname, './'),
  //   hot: true,
  //   port: 9000,
  //   historyApiFallback: true
  // },
  plugins: [
    new MomentLocalesPlugin(),
    new CleanWebpackPlugin(),
    new webpack.DefinePlugin({
      'process.env.LANDSCAPE_STREAM': JSON.stringify(process.env.LANDSCAPE_STREAM),
      'process.env.LANDSCAPE_SHORTHASH': JSON.stringify(process.env.LANDSCAPE_SHORTHASH),
      'process.env.TUTORIAL_HOST': JSON.stringify('~hastuc-dibtux'),
      'process.env.TUTORIAL_GROUP': JSON.stringify('beginner-island'),
      'process.env.TUTORIAL_CHAT': JSON.stringify('chat-8401'),
      'process.env.TUTORIAL_BOOK': JSON.stringify('notebook-9148'),
      'process.env.TUTORIAL_LINKS': JSON.stringify('links-4353'),
    }),
    // new HtmlWebpackPlugin({
    //   title: 'Hot Module Replacement',
    //   template: './public/index.html',
    // }),
  ],
  output: {
    filename: 'index.[contenthash].js',
    path: path.resolve(__dirname, '../../arvo/app/landscape/js/bundle'),
    publicPath: '/'
  },
  optimization: {
    minimize: true,
    usedExports: true
  }
};
