# Urbit ESLint + Prettier Configuration

## Abstract

These are our shared settings for [ESLint](https://eslint.org/) and [Prettier](https://prettier.io). This ESLint configuration lints and formats our code so it follows our shared style guide. It uses ESLint to lint and fix Javascript, and Prettier to fix issues and format our code nicely. 😇

## Installation

Install the package with:

```sh
yarn add @urbit/eslint-config --dev
```

`@urbit/eslint-config` requires you to take care of it's `peerDependencies`. Install the correct version of each `peerDependencies` package, which are listed with the following command:

```sh
npx install-peerdeps --dev @urbit/eslint-config
```

## ESLint Configuration

Now add `@urbit/eslint-config` to your `.eslintrc.js`:

```js
// .eslintrc.js
module.exports = {
    extends: '@urbit/eslint-config',
}
```

## Prettier Configuration

This is how you can use or extend the `@urbit/eslint-config` Prettier config in your app:

```js
// .prettierrc.js
module.exports = require('@urbit/eslint-config/.prettierrc.js')

// or to override specific options
module.exports = {
    ...require('@urbit/eslint-config/.prettierrc.js'),
    semi: true,
}
```
