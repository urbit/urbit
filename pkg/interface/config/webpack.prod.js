const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
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
        test: /\.(sc|c)ss$/i,
        use: [
          // Creates `style` nodes from JS strings
          'style-loader',
          // Translates CSS into CommonJS
          'css-loader',
          // Compiles Sass to CSS
          'sass-loader'
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[name].[ext]',
              outputPath: 'fonts/'
            }
          }
        ]
      },
      {
        test: [/\.bmp$/, /\.gif$/, /\.jpe?g$/, /\.png$/],
        loader: require.resolve('url-loader'),
          options: {
          limit: 10000,
          name: 'static/media/[name].[hash:8].[ext]'
        }
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
      'process.env.LANDSCAPE_SHORTHASH': JSON.stringify(GIT_DESC),
      'process.env.LANDSCAPE_STORAGE_VERSION': Date.now().toString(),
      'process.env.LANDSCAPE_LAST_WIPE': '2021-10-20'
    }),
    new HtmlWebpackPlugin({
      title: 'EScape',
      template: './public/index.html',
      favicon: './src/assets/img/Favicon.png'
    })
  ],
  output: {
    filename: (pathData) => {
      return pathData.chunk.name === 'app' ? 'index.[contenthash].js' : '[name].js';
    },
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/apps/escape/'
  },
  optimization: {
    minimize: true,
    usedExports: true
  }
};
