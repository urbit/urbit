const _ = require("lodash");
const path = require("path");

const isDev = process.env.NODE_ENV !== "production";

function devServer(base, urbitrc = {}) {
  const router = _.mapKeys(
    urbitrc.FLEET || {},
    (_v, k) => `${key}.localhost:9000`
  );
  const proxy = !urbitrc.URL
    ? {}
    : {
        index: "index.html",
        proxy: [
          {
            context: path => {
              if(path === `/apps/${base}/desk.js`) {
                return true;
              }
              return !path.startsWith(`/apps/${base}`);
            },
            changeOrigin: true,
            target: urbitrc.URL,
            router,
          },
        ],
      };
  return {
    hot: true,
    port: 9000,
    host: "0.0.0.0",
    disableHostCheck: true,
    historyApiFallback: true,
    publicPath: `/apps/${base}/`,
    ...proxy,
  };
}

function loadTsDev() {
  return [
    {
      test: /\.(j|t)sx?$/,
      use: {
        loader: "babel-loader",
        options: {
          presets: [
            "@babel/preset-env",
            "@babel/typescript",
            [
              "@babel/preset-react",
              {
                runtime: "automatic",
                development: true,
                importSource: "@welldone-software/why-did-you-render",
              },
            ],
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
      exclude:
        /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light|@tlon\/indigo-react)\/).*/,
    },
  ];
}

function loadTsProd() {
  return [
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
            "lodash",
            "@babel/transform-runtime",
            "@babel/plugin-proposal-object-rest-spread",
            "@babel/plugin-proposal-optional-chaining",
            "@babel/plugin-proposal-class-properties",
          ],
        },
      },
      exclude:
        /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light|@tlon\/indigo-react)\/).*/,
    },
  ];
}

const optimization = isDev
  ? undefined
  : {
      minimize: true,
      usedExports: true,
      runtimeChunk: "single",
      splitChunks: {
        cacheGroups: {
          vendor: {
            test: /[\\/]node_modules[\\/]/,
            name: "vendors",
            chunks: "all",
          },
        },
      },
    };

const config = (base, urbitrc) => ({
  mode: isDev ? "development" : "production",
  entry: {
    app: "./src/index.js",
  },
  module: {
    rules: [
      ...(isDev ? loadTsDev() : loadTsProd()),
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
      {
        test: /\.(woff2|png|svg|jpg|jpeg)/i,
        type: "asset/resource",
      },
    ],
  },
  resolve: {
    extensions: [".js", ".ts", ".tsx"],
    fallback: {
      path: false,
      http: false,
      stream: false,
      fs: false
    },
  },
  devtool: isDev ? "inline-source-map" : "source-map",
  devServer: devServer(base, urbitrc),
  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(process.cwd(), "./dist"),
    publicPath: `/apps/${base}/`,
    clean: true,
  },
  optimization,
});

module.exports = {
  mode: isDev ? "development" : "production",
  config,
  loadTs: isDev ? loadTsDev : loadTsProd,
  devServer,
};
