#!/bin/sh

install_rust_packages() {
  local githubPackageFile="$DOTFILES_PACKAGES_DIR/rust_repo_urls.txt"
  local url

  log_info "Installing Rust packages from GitHub"
  while read -r url || [ -n "$url" ]; do
    cargo install --git "$url"
  done < "$githubPackageFile"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
