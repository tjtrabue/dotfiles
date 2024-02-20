/**
 * Configuration file for the Prettier Javascript auto-formatter.
 */

module.exports = {
  semi: true,
  singleQuote: true,
  quoteProps: 'consistent',
  jsxSingleQuote: false,
  trailingComma: 'es5',
  arrowParens: 'avoid',
  bracketSpacing: false,
  plugins: ['prettier-plugin-tailwindcss'],
  // Always wrap Markdown files after a fixed width
  printWidth: 80,
  proseWrap: 'always',
};

// vim:foldenable:foldmethod=marker
