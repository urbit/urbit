module.exports = {
  stories: ['../src/**/*.stories.mdx', '../src/**/*.stories.@(js|jsx|ts|tsx)'],
  addons: ['@storybook/addon-links', '@storybook/addon-essentials'],
  webpackFinal: (config) => {
    config.module.rules.push({
      test: /\.(j|t)sx?$/,
      use: {
        loader: 'babel-loader',
        options: {
          presets: [
            '@babel/preset-env',
            '@babel/typescript',
            '@babel/preset-react',
          ],
          plugins: [
            '@babel/transform-runtime',
            '@babel/plugin-proposal-object-rest-spread',
            '@babel/plugin-proposal-optional-chaining',
            '@babel/plugin-proposal-class-properties',
          ],
        },
      },
      exclude: /node_modules\/(?!(@tlon\/indigo-dark|@tlon\/indigo-light|@tlon\/indigo-react)\/).*/,
    });
    return config;
  },
};

