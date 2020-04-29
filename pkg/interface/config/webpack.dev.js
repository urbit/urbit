const path = require('path');
// const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');

module.exports = {
  mode: 'development',
  entry: {
     app: './src/index.js'
  },
  module: {
    rules: [
      {
        test: /\.js?$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env', '@babel/preset-react'],
            plugins: [
              '@babel/plugin-proposal-object-rest-spread',
              '@babel/plugin-proposal-optional-chaining'
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
    extensions: ['.js']
  },
  devtool: 'inline-source-map',
  // devServer: {
  //   contentBase: path.join(__dirname, './'),
  //   hot: true,
  //   port: 9000,
  //   historyApiFallback: true
  // },
  plugins: [
    // new CleanWebpackPlugin(),
    // new HtmlWebpackPlugin({
    //   title: 'Hot Module Replacement',
    //   template: './public/index.html',
    // }),
  ],
  output: {
    filename: 'index.js',
    chunkFilename: 'index.js',
    path: path.resolve(urbitrc.URBIT_PIERS[0] + '/app/launch/', 'js'),
    publicPath: '/'
  },
  optimization: {
    minimize: false,
    usedExports: true
  }
};
