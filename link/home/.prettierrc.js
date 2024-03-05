/**
 * Configuration file for the Prettier Javascript auto-formatter.
 */

const config = {
  semi: true,
  singleQuote: true,
  quoteProps: 'consistent',
  jsxSingleQuote: false,
  trailingComma: 'es5',
  arrowParens: 'avoid',
  bracketSpacing: false,
  plugins: ['prettier-plugin-tailwindcss'],
  proseWrap: 'always',
};

export default config;

// vim:foldenable:foldmethod=marker
