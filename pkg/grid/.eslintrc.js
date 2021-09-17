module.exports = {
  root: true,
  env: {
    browser: true,
    es2021: true
  },
  extends: [
    'plugin:react/recommended',
    'airbnb',
    'plugin:@typescript-eslint/recommended',
    'plugin:import/errors',
    'plugin:jsx-a11y/recommended',
    'plugin:tailwind/recommended',
    'prettier',
    'prettier/prettier',
    'plugin:prettier/recommended'
  ],
  ignorePatterns: ['**/*.config.js', '**/*.config.ts'],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaFeatures: {
      jsx: true
    },
    ecmaVersion: 12,
    sourceType: 'module'
  },
  plugins: ['react', '@typescript-eslint', 'import', 'jsx-a11y', 'react-hooks'],
  rules: {
    'no-undef': 'off',
    'no-unused-vars': 'off',
    '@typescript-eslint/no-unused-vars': ['error'],
    'no-unused-expressions': ['error', { allowShortCircuit: true }],
    'no-use-before-define': 'off',
    'no-param-reassign': ['error', { props: true, ignorePropertyModificationsFor: ['draft'] }],
    '@typescript-eslint/no-use-before-define': 'off',
    '@typescript-eslint/no-empty-function': 'off',
    'react/jsx-filename-extension': ['warn', { extensions: ['.tsx'] }],
    'import/extensions': [
      'error',
      'ignorePackages',
      {
        ts: 'never',
        tsx: 'never'
      }
    ],
    'no-shadow': 'off',
    '@typescript-eslint/no-shadow': ['error'],
    'react-hooks/rules-of-hooks': 'error',
    'react-hooks/exhaustive-deps': 'off',
    'react/no-array-index-key': 'off',
    'import/prefer-default-export': 'off',
    'react/prop-types': 'off',
    'react/jsx-props-no-spreading': 'off',
    'react/require-default-props': 'off',
    'import/no-extraneous-dependencies': ['error'],
    'tailwind/class-order': 'off',
    'jsx-a11y/label-has-associated-control': [
      'error',
      {
        required: {
          some: ['nesting', 'id']
        }
      }
    ],
    'jsx-a11y/label-has-for': [
      'error',
      {
        required: {
          some: ['nesting', 'id']
        }
      }
    ]
  },
  settings: {
    'import/parsers': {
      '@typescript-eslint/parser': ['.ts']
    },

    'import/resolver': {
      typescript: {
        alwaysTryTypes: true
      }
    }
  }
};
