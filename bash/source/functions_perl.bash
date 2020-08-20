#!/usr/bin/env bash

# Install plsense intellisense engine for Perl.
# As of now it is not on CPAN.
install_plsense() {
  local version="${1:-0.3.4}"
  # URL to the release page for plsense.
  local base_url="https://github.com/aki2o/plsense/releases/download"
  # Full download URL.
  local full_url="${base_url}/v${version}/PlSense-${version}.tar.gz"

  if cpanm -i --force "$full_url"; then
    # Add plsense configuration after installing.
    plsense config
  fi
}

install_perl_packages() {
  local package

  while read -r package || [ -n "$package" ]; do
    cpanm -i --force "$package"
  done <"$PERL_PACKAGES_FILE"

  install_plsense
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
