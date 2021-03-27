const path = require('path');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');

module.exports = {
  mode: 'production',
  entry: {
     app: './src/index.js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env', '@babel/preset-react'],
            plugins: [
              '@babel/transform-runtime',
              '@babel/plugin-proposal-object-rest-spread',
              '@babel/plugin-proposal-optional-chaining',
              '@babel/plugin-proposal-class-properties'
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
  plugins: [
    new CleanWebpackPlugin()
  ],
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, `../../arvo/app/btc-wallet/js/bundle`),
    publicPath: '/',
  },
  optimization: {
    minimize: true,
    usedExports: true
  }
};
