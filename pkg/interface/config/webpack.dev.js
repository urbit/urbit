const path = require('path');
const webpack = require('webpack');
// const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require('./urbitrc');
const _ = require('lodash');
const { execSync } = require('child_process');

const GIT_DESC = execSync('git describe --always', { encoding: 'utf8' }).trim();

class UrbitShipPlugin {
  constructor(urbitrc) {
    this.piers = urbitrc.URBIT_PIERS;
    this.herb = urbitrc.herb || false;
  }

  apply(compiler) {
    compiler.hooks.afterEmit.tapPromise(
      'UrbitShipPlugin',
      async (compilation) => {
        // const src = path.resolve(compiler.options.output.path, 'index.js');
        // uncomment to copy into all piers
        //
        // return Promise.all(this.piers.map(pier => {
        //   const dst = path.resolve(pier, 'app/landscape/js/index.js');
        //   copyFile(src, dst).then(() => {
        //     if(!this.herb) {
        //       return;
        //     }
        //     pier = pier.split('/');
        //     const desk = pier.pop();
        //     return exec(`herb -p hood -d '+hood/commit %${desk}' ${pier.join('/')}`);
        //   });
        // }));
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
  historyApiFallback: true
};

const router =  _.mapKeys(urbitrc.FLEET || {}, (value, key) => `${key}.localhost:9000`);

if(urbitrc.URL) {
  devServer = {
    ...devServer,
    index: '',
    proxy: {
      '/~landscape/js/bundle/index.*.js': {
        target: 'http://localhost:9000',
        pathRewrite: (req, path) => {
          return '/index.js';
        }
      },
      // '/~landscape/js/serviceworker.js': {
      //   target: 'http://localhost:9000',
      //   pathRewrite: (req, path) => {
      //     return '/serviceworker.js'
      //   }
      // },
      '**': {
        changeOrigin: true,
        target: urbitrc.URL,
        router,
        // ensure proxy doesn't timeout channels
        proxyTimeout: 0
     }
    }
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
      },
      {
        test: /\.(woff|woff2|ttf)$/i,
        use: ['url-loader']
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
    new webpack.DefinePlugin({
      'process.env.LANDSCAPE_SHORTHASH': JSON.stringify(GIT_DESC),
      'process.env.TUTORIAL_HOST': JSON.stringify('~difmex-passed'),
      'process.env.TUTORIAL_GROUP': JSON.stringify('beginner-island'),
      'process.env.TUTORIAL_CHAT': JSON.stringify('introduce-yourself-7010'),
      'process.env.TUTORIAL_BOOK': JSON.stringify('guides-9684'),
      'process.env.TUTORIAL_LINKS': JSON.stringify('community-articles-2143')
    })

    // new CleanWebpackPlugin(),
    // new HtmlWebpackPlugin({
    //   title: 'Hot Module Replacement',
    //   template: './public/index.html',
    // }),
  ],
  watch: true,
  output: {
    filename: (pathData) => {
      return pathData.chunk.name === 'app' ? 'index.js' : '[name].js';
    },
    chunkFilename: '[name].js',
    path: path.resolve(__dirname, '../dist'),
    publicPath: '/',
    globalObject: 'this'
  },
  optimization: {
    minimize: false,
    usedExports: true
  }
};
