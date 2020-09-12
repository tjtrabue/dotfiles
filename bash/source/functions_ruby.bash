#!/usr/bin/env bash

install_ruby_packages() {
  local line
  while read -r line || [ -n "$line" ]; do
    gem install "$line"
  done <"$RUBY_PACKAGES_FILE"
}

update_ruby_packages() {
  gem update
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
