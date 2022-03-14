// @mwvd note:
// using .prettierrc.js (instead of .prettierrc)
// will allow us to import a shared config later
// if we like with
// module.exports = require('@urbit/eslint-config/.prettierrc.js')

module.exports = {
  bracketSpacing: true,
  jsxBracketSameLine: true,
  printWidth: 80,
  proseWrap: 'always',
  semi: false,
  singleQuote: true,
  tabWidth: 2,
  trailingComma: 'all',
  useTabs: false,
  overrides: [
    {
      files: ['*.jsx', '*.tsx', '*.html', '*.css'],
      options: {
        tabWidth: 4,
      },
    },
    {
      files: ['*.html'],
      htmlWhitespaceSensitivity: 'ignore',
      // https://github.com/prettier/prettier-vscode/issues/646#issuecomment-514776589
    },
    {
      files: ['*.md'],
      options: {
        tabWidth: 4,
        printWidth: 1000,
      },
    },
    {
      files: ['package*.json'],
      options: {
        printWidth: 1000,
      },
    },
    {
      files: ['*.yaml', '*.yml'],
      options: {
        singleQuote: false,
      },
    },
  ],
}
