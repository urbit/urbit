const env = {
  "browser": true,
  "es6": true,
  "node": true
};

const rules = {
  "array-bracket-spacing": ["error", "never"],
  "arrow-parens": [
    "error",
    "as-needed",
    {
      "requireForBlockBody": true
    }
  ],
  "arrow-spacing": "error",
  "block-spacing": ["error", "always"],
  "brace-style": ["error", "1tbs"],
  "camelcase": [
    "error",
    {
      "properties": "never"
    }
  ],
  "comma-dangle": ["error", "never"],
  "eol-last": ["error", "always"],
  "func-name-matching": "error",
  "indent": [
    "off",
    2,
    {
      "ArrayExpression": "off",
      "SwitchCase": 1,
      "CallExpression": {
        "arguments": "off"
      },
      "FunctionDeclaration": {
        "parameters": "off"
      },
      "FunctionExpression": {
        "parameters": "off"
      },
      "MemberExpression": "off",
      "ObjectExpression": "off",
      "ImportDeclaration": "off"
    }
  ],
  "handle-callback-err": "off",
  "linebreak-style": ["error", "unix"],
  "max-lines": [
    "error",
    {
      "max": 300,
      "skipBlankLines": true,
      "skipComments": true
    }
  ],
  "max-lines-per-function": [
    "warn",
    {
      "skipBlankLines": true,
      "skipComments": true
    }
  ],
  "max-statements-per-line": [
    "error",
    {
      "max": 1
    }
  ],
  "new-cap": [
    "error",
    {
      "newIsCap": true,
      "capIsNew": false
    }
  ],
  "new-parens": "error",
  "no-buffer-constructor": "error",
  "no-console": "off",
  "no-extra-semi": "off",
  "no-fallthrough": "off",
  "no-func-assign": "off",
  "no-implicit-coercion": "error",
  "no-multi-assign": "error",
  "no-multiple-empty-lines": [
    "error",
    {
      "max": 1
    }
  ],
  "no-nested-ternary": "error",
  "no-param-reassign": "off",
  "no-return-assign": "error",
  "no-return-await": "off",
  "no-shadow-restricted-names": "error",
  "no-tabs": "error",
  "no-trailing-spaces": "error",
  "no-unused-vars": "off",
  "@typescript-eslint/no-unused-vars": [
    "error",
    {
      "vars": "all",
      "args": "none",
      "ignoreRestSiblings": false
    }
  ],
  "no-use-before-define": "off",
  "@typescript-eslint/no-use-before-define": [
    "error",
    {
      "functions": false,
      "classes": false
    }
  ],
  "no-useless-escape": "off",
  "no-var": "error",
  "nonblock-statement-body-position": ["error", "below"],
  "object-curly-spacing": ["error", "always"],
  "padded-blocks": ["error", "never"],
  "prefer-arrow-callback": "error",
  "prefer-const": [
    "error",
    {
      "destructuring": "all",
      "ignoreReadBeforeAssign": true
    }
  ],
  "prefer-template": "off",
  "quotes": ["error", "single"],
  "semi": ["error", "always"],
  "spaced-comment": [
    "error",
    "always",
    {
      "exceptions": ["!"]
    }
  ],
  "space-before-blocks": "error",
  "unicode-bom": ["error", "never"],
  "valid-jsdoc": "error",
  "wrap-iife": ["error", "inside"],
  "react/jsx-closing-bracket-location": 1,
  "react/jsx-tag-spacing": 1,
  "react/jsx-max-props-per-line": ["error", { "maximum": 2, "when": "multiline" }],
  "react/prop-types": 0
};

module.exports = {
  "env": env,
  "extends": [
    "plugin:react/recommended",
    "eslint:recommended",
  ],
  "settings": {
    "react": {
      "version": "^16.5.2"
    }
  },
  "parser": "babel-eslint",
  "parserOptions": {
    "ecmaVersion": 10,
    "requireConfigFile": false,
    "sourceType": "module"
  },
  "root": true,
  "rules": rules,
  "overrides": [
    {
      "files": ["**/*.ts", "**/*.tsx"],
      "env": env,
      "extends": [
        "eslint:recommended",
        "plugin:@typescript-eslint/eslint-recommended",
        "plugin:@typescript-eslint/recommended"
      ],
      "parser": "@typescript-eslint/parser",
      "parserOptions": {
        "ecmaFeatures": { "jsx": true },
        "ecmaVersion": 10,
        "requireConfigFile": false,
        "sourceType": "module"
      },
      "plugins": ["@typescript-eslint"],
      "rules": rules
    }
  ]
};
