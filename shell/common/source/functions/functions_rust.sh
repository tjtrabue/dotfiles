#!/bin/sh

install_rust_packages() {
  local packageFile="${DOTFILES_PACKAGES_DIR}/rust_packages.txt"
  local package

  log_info "Installing Rust Packages"
  while read -r package || [ -n "${package}" ]; do
    cargo install "${package}"
  done <"${packageFile}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
