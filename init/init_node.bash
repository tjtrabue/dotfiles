#!/usr/bin/env bash

# Primary Functions {{{
# Set the global NPM configuration.
configure_node() {
  mkdir -p "$NPM_GLOBAL_PACKAGES_DIR";
  npm config set prefix "$NPM_GLOBAL_PACKAGES_DIR";
}

# Install all packages
install_node_global_packages() {
  xargs -0 npm install -g < <(tr \\n \\0 <"$NPM_GLOBAL_PACKGES_DIR");
}
# }}}

# Variables {{{
declare PACKAGE_FILE="$INIT_DIR/package_files/npm_global_packages.txt";
declare NPM_GLOBAL_PACKAGES_DIR="$HOME/.npm-global";
# }}}

main() {
  configure_node;
  install_node_global_packages;
}
main;

# vim:foldenable:foldmethod=marker:
