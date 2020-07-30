const path = require("path");
// const HtmlWebpackPlugin = require('html-webpack-plugin');
// const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const urbitrc = require("./urbitrc");
const fs = require("fs");
const util = require("util");
const exec = util.promisify(require("child_process").exec);

function copyFile(src, dest) {
  return new Promise((res, rej) =>
    fs.copyFile(src, dest, (err) => (err ? rej(err) : res()))
  );
}

class UrbitShipPlugin {
  constructor(urbitrc) {
    this.piers = urbitrc.URBIT_PIERS;
    this.herb = urbitrc.herb || false;
  }

  apply(compiler) {
    compiler.hooks.afterEmit.tapPromise(
      "UrbitShipPlugin",
      async (compilation) => {
        const src = path.resolve(compiler.options.output.path, "index.js");
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
  contentBase: path.join(__dirname, "../dist"),
  hot: true,
  port: 9000,
  historyApiFallback: true,
};

if (urbitrc.URL) {
  devServer = {
    ...devServer,
    index: "",
    headers: {
      "Access-Control-Allow-Origin": "capacitor://localhost",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
      "Access-Control-Allow-Credentials": "*",
    },
    writeToDisk: true,
    proxy: {
      "/js/index.js": {
        target: "http://localhost:9000",
        pathRewrite: (req, path) => "/index.js",
      },
      "**": {
        target: urbitrc.URL,
        // ensure proxy doesn't timeout channels
        proxyTimeout: 0,
        headers: {
          "Access-Control-Allow-Origin": 'capacitor://localhost',
          "Access-Control-Allow-Credentials": 'true'
        },
        onProxyRes: (proxyRes, req, res) => {
          console.log(proxyRes);
          res.setHeader('access-control-allow-origin', 'capacitor://localhost')
          res.setHeader('access-control-allow-credentials', 'true')
          if (req.headers["access-control-request-method"]) {
            res.setHeader(
              "access-control-allow-methods",
              req.headers["access-control-request-method"]
            );
          }

          if (req.headers["access-control-request-headers"]) {
            res.setHeader(
              "access-control-allow-headers",
              req.headers["access-control-request-headers"]
            );
          }

          res.setHeader("access-control-max-age", 60 * 60 * 24 * 30);
          if (req.method === "OPTIONS") {
            res.send(200);
            res.end();
          }
        },
      },
    },
  };
}

module.exports = {
  mode: "development",
  entry: {
    app: "./src/index.js",
  },
  module: {
    rules: [
      {
        test: /\.(j|t)sx?$/,
        use: {
          loader: "babel-loader",
          options: {
            presets: [
              "@babel/preset-env",
              "@babel/typescript",
              "@babel/preset-react",
            ],
            plugins: [
              "@babel/transform-runtime",
              "@babel/plugin-proposal-object-rest-spread",
              "@babel/plugin-proposal-optional-chaining",
              "@babel/plugin-proposal-class-properties",
              "react-hot-loader/babel",
            ],
          },
        },
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: [
          // Creates `style` nodes from JS strings
          "style-loader",
          // Translates CSS into CommonJS
          "css-loader",
          // Compiles Sass to CSS
          "sass-loader",
        ],
      },
    ],
  },
  resolve: {
    extensions: [".js", ".ts", ".tsx"],
  },
  devtool: "inline-source-map",
  devServer: devServer,
  plugins: [
    new UrbitShipPlugin(urbitrc),
    // new CleanWebpackPlugin(),
    // new HtmlWebpackPlugin({
    //   title: 'Hot Module Replacement',
    //   template: './public/index.html',
    // }),
  ],
  watch: true,
  output: {
    filename: "index.js",
    chunkFilename: "index.js",
    path: path.resolve(__dirname, "../dist"),
    publicPath: "/",
  },
  optimization: {
    minimize: false,
    usedExports: true,
  },
};
