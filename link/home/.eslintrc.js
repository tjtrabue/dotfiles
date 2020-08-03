/**
 * Configuration file for the ESLint Javascript code checker.
 */

module.exports = {
  parser: "babel-eslint", // Use Babel-ESLint Javascript parser for new features.
  parserOptions: {
    ecmaVersion: 10,
    sourceType: "module",
    exmaFeatures: {
      globalReturn: true,
      impliedStrict: true,
      jsx: true,
    }
  },

  // Specify particular JS environments to apply
  env: {
    "browser": true,
    "node": true,
    "es6": true,
    "jasmine": true,
    "jest/globals": true,
  },

  // Additional "config" blocks
  extends: [
    "eslint:recommended",
    "plugin:angular/johnpapa",
    "plugin:jest/recommended",
    "plugin:node/recommended",
    "plugin:prettier/recommended",
    "plugin:promise/recommended",
    "plugin:react/recommended",
  ],

  // Additional plugins
  plugins: [
    "angular",
    "filenames",
    "html",
    "jest",
    "jsx-a11y",
    "node",
    "prettier",
    "promise",
    "react",
    "react-hooks",
  ],

  // ESLint code checker rules
  rules: {
    // Possible Errors
    "no-async-promise-executor": "error",
    "no-await-in-loop": "error",
    "no-extra-parens": "error",
    "no-template-curly-in-string": "error",

    // Best Practices
    "curly": "error",
    "dot-location": "error",
    "dot-notation": "error",
    "eqeqeq": "error",
    "no-else-return": "error",
    "no-extra-bind": "error",
    "no-extra-label": "error",
    "no-floating-decimal": "error",
    "no-implicit-coercion": "error",
    "no-multi-spaces": "error",
    "no-useless-return": "error",
    "wrap-iife": "error",
    "yoda": "error",

    // Strict Mode
    "strict": "error",

    // Variables
    "no-label-var": "error",
    "no-shadow": ["error", {
      "builtinGlobals": true, // Don't allow shadowing of global vars
      "hoist": "functions", // Report shadowing before the outer functions are defined
      "allow": [], // Extra variable names for which to allow shadowing
    }],
    "no-undef-init": "error",
    "no-use-before-define": "error",

    // Node.js and CommonJS
    "callback-return": "error",
    "global-require": "error",
    "handle-callback-err": "error",
    "no-new-require": "error",

    // Stylistic Issues
    "array-bracket-newline": "error",
    "array-bracket-spacing": "error",
    "array-element-newline": ["error", "consistent"],
    "block-spacing": ["error", "always"],
    "brace-style": ["error", "1tbs"],
    "camelcase": ["error", {
      "properties": "never",
      "ignoreDestructuring": false,
    }],
    "capitalized-comments": ["error", "always"],
    "comma-dangle": ["error", "always-multiline"],
    "comma-spacing": ["error", {
      "before": false,
      "after": true,
    }],
    "comma-style": ["error", "last"],
    "computed-property-spacing": ["error", "never"],
    "consistent-this": "error",
    "eol-last": ["error", "always"],
    "func-call-spacing": ["error", "never"],
    "func-name-matching": ["error", "always"],
    "func-names": ["error", "always"],
    "func-style": ["error", "declaration"],
    "function-paren-newline": ["error", "multiline"],
    "implicit-arrow-linebreak": ["error", "beside"],
    "multiline-comment-style": ["error", "starred-block"],
    "new-parens": "error",
    "no-lonely-if": "error",
    "no-multiple-empty-lines": ["error", {
      "max": 1,
    }],
    "no-trailing-spaces": "error",
    "no-unneeded-ternary": "error",
    "no-whitespace-before-property": "error",
    "nonblock-statement-body-position": ["error", "beside"],
    "object-curly-newline": "error",
    "object-curly-spacing": "error",
    "object-property-newline": ["error",  {"allowAllPropertiesOnSameLine": true}],
    "one-var": ["error", "never"],
    "one-var-declaration-per-line": ["error", "always"],
    "operator-assignment": ["error", "always"],
    "operator-linebreak": ["error", "before"],
    "padded-blocks": ["error", "never"],
    "prefer-object-spread": "error",
    "quote-props": ["error", "consistent-as-needed"],
    "quotes": ["error", "double", {
      "avoidEscape": true,
      "allowTemplateLiterals": true,
    }],
    "semi": "error",
    "semi-spacing": ["error", {"before": false, "after": true}],
    "semi-style": ["error", "last"],
    "sort-vars": "error",
    "space-before-blocks": ["error", "always"],
    "space-before-function-paren": ["error", "never"],
    "space-in-parens": ["error", "never"],
    "space-infix-ops": "error",
    "space-unary-ops": ["error", {
      "words": true,
      "nonwords": false,
    }],
    "spaced-comment": ["error", "always"],
    "switch-colon-spacing": "error",
    "template-tag-spacing": ["error", "always"],
    "unicode-bom": ["error", "never"],
    "wrap-regex": "error",

    // ECMAScript 6
    "arrow-body-style": ["error", "as-needed"],
    "arrow-parens": ["error", "as-needed"],
    "arrow-spacing": ["error", {
      "before": true,
      "after": true,
    }],
    "generator-star-spacing": ["error", "before"],
    "no-confusing-arrow": ["error", {"allowParens": false}],
    "no-useless-computed-key": "error",
    "no-useless-constructor": "error",
    "no-useless-rename": "error",
    "no-var": "error",
    "object-shorthand": ["error", "always"],
    "prefer-arrow-callback": "error",
    "prefer-const": ["error", {
      "destructuring": "any",
      "ignoreReadBeforeAssign": false,
    }],
    "prefer-destructuring": ["error", {
      "array": true,
      "object": true,
    }, {
      "enforceForRenamedProperties": false,
    }],
    "prefer-numeric-literals": "error",
    "prefer-rest-params": "error",
    "prefer-spread": "error",
    "prefer-template": "error",
    "rest-spread-spacing": ["error", "never"],
    "sort-imports": ["error", {
      "ignoreCase": false,
      "ignoreMemberSort": false,
      "memberSyntaxSortOrder": ["none", "all", "multiple", "single"],
    }],
    "template-curly-spacing": ["error", "never"],
    "yield-star-spacing": ["error", "after"],

    // Prettier
    "prettier/prettier": ["error", {}, {
      "usePrettierrc": true,
    }],
  },
}

// vim:foldmethod=marker
