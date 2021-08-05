const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MomentLocalesPlugin = require('moment-locales-webpack-plugin');
const webpack = require('webpack');
const { execSync } = require('child_process');

const GIT_DESC = execSync('git describe --always', { encoding: 'utf8' }).trim();

module.exports = {
  mode: 'production',
  entry: {
     app: './src/index.js',
     serviceworker: './src/serviceworker.js'
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
        exclude: /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light|@tlon\/indigo-react|@urbit\/api)\/).*/
      },
      {
         test: /\.css$/i,
        use: [
          // Translates CSS into CommonJS
          'css-loader',
          // Compiles Sass to CSS
          'sass-loader'
        ]
      },
      {
        test: /\.woff2$/i,
        type: 'asset/resource'
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx'],
    fallback: {
      path: false,
      http: false
    }
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
    new webpack.DefinePlugin({
      'process.env.LANDSCAPE_STREAM': JSON.stringify(process.env.LANDSCAPE_STREAM),
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
    })
  ],
  output: {
    filename: '[name].[contenthash].js',
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/apps/landscape',
    clean: true
  },
  optimization: {
    minimize: true,
    usedExports: true,
    runtimeChunk: 'single',
    splitChunks: {
      cacheGroups: {
        vendor: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all'
        }
      }
    }
  }
};
