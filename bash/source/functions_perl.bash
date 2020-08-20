#!/usr/bin/env bash

install_perl_packages() {
  local package

  while read -r package || [ -n "$package" ]; do
    cpanm -i --force "$package"
  done <"$PERL_PACKAGES_FILE"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
