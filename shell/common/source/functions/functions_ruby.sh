#!/bin/sh

install_ruby_packages() {
  local line
  while read -r line || [ -n "$line" ]; do
    gem install "$line"
  done <"$RUBY_PACKAGES_FILE"
}

add_gem_bin_dirs_to_path() {
  local gemHomeDir="${HOME}/.gem/ruby"
  local versionedBinDir

  while read -r versionedBinDir || [ -n "${versionedBinDir}" ]; do
    log_info "Adding Ruby Gem dir ${versionedBinDir} to PATH"
    atp "${versionedBinDir}"
  done <<<"$(find "${gemHomeDir}" -maxdepth 1 -mindepth 1 -type d \
    -exec echo '{}'/bin \;)"
}

update_ruby_packages() {
  gem update
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
