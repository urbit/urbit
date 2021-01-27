const path = require('path');
const webpack = require('webpack');

const shared = {
  mode: 'production',
  entry: {
     app: './src/index.ts'
  },
  module: {
    rules: [
      {
        test: /\.(j|t)s$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/typescript'],
            plugins: [
              '@babel/plugin-proposal-class-properties',
              '@babel/plugin-proposal-object-rest-spread',
              '@babel/plugin-proposal-optional-chaining',
            ],
          }
        },
        exclude: /node_modules/
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.ts', '.ts'],
    fallback: {
      fs: false,
      child_process: false,
      util: require.resolve("util/"),
      buffer: require.resolve('buffer/'),
      assert: false,
      http: require.resolve('stream-http'),
      https: require.resolve('stream-http'),
      stream: require.resolve('stream-browserify'),
      zlib: require.resolve("browserify-zlib"),
    }
  },
  
  optimization: {
    minimize: false,
    usedExports: true
  }
};

const serverConfig = {
  ...shared,
  target: 'node',
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'dist'),
    library: 'Urbit',
    libraryExport: 'default'
  },
  plugins: [
    new webpack.ProvidePlugin({
      XMLHttpRequest: ['xmlhttprequest-ssl', 'XMLHttpRequest'],
      EventSource: 'eventsource',
      fetch: ['node-fetch', 'default'],
    }),
  ],
};

const browserConfig = {
  ...shared,
  target: 'web',
  output: {
    filename: 'browser.js',
    path: path.resolve(__dirname, 'dist'),
    library: 'Urbit',
    libraryExport: 'default'
  },
  plugins: [
    new webpack.ProvidePlugin({
      Buffer: 'buffer',
    }),
  ],
};


const exampleBrowserConfig = {
  ...shared,
  mode: 'development',
  entry: {
     app: './src/example/browser.js'
  },
  output: {
    filename: 'browser.js',
    path: path.resolve(__dirname, 'example'),
  }
};

const exampleNodeConfig = {
  ...shared,
  mode: 'development',
  target: 'node',
  entry: {
     app: './src/example/node.js'
  },
  output: {
    filename: 'node.js',
    path: path.resolve(__dirname, 'example'),
  }
};

module.exports = [ serverConfig, browserConfig, exampleBrowserConfig, exampleNodeConfig ];