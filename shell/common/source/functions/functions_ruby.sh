#!/bin/sh

install_ruby_packages() {
  local line

  while IFS='' read -r line || [ -n "${line}" ]; do
    gem install "${line}"
  done <"${RUBY_PACKAGES_FILE}"
}

add_gem_bin_dirs_to_path() {
  local gemHomeDir="${HOME}/.gem/ruby"
  local versionedBinDir

  while IFS='' read -r versionedBinDir || [ -n "${versionedBinDir}" ]; do
    log_info "Adding Ruby Gem dir ${versionedBinDir} to PATH"
    atp "${versionedBinDir}"
  done <<<"$(find "${gemHomeDir}" -maxdepth 1 -mindepth 1 -type d \
    -exec echo '{}'/bin \;)"
}

# Install Ruby Version Manager (rvm) for any supported platform.
install_rvm() {
  if ! __rvm_installed; then
    curl -sSL "https://get.rvm.io" | bash
  else
    warn "rvm is already installed."
  fi
}

# Use rvm to install the latest ruby.
install_latest_ruby() {
  local rvmHome="${HOME}/.rvm"
  local rvmScriptsDir="${rvmHome}/scripts"

  if ! __rvm_installed; then
    # Install rvm if necessary.
    install_rvm
    . "${rvmScriptsDir}/rvm"
  elif [ ! -x "$(command -v rvm)" ]; then
    # If we have the rvm directory, just source the rvm script.
    . "${rvmScriptsDir}/rvm"
  fi

  rvm install ruby --latest
}

# Update all Ruby Gems.
update_ruby_packages() {
  gem update
}

# Return 0 if rvm is installed. Return non-zero otherwise.
__rvm_installed() {
  local rvmHome="${HOME}/.rvm"
  if [ "$(command -v rvm)" = "" ] && [ ! -d "${rvmHome}" ]; then
    return 1
  fi
  return 0
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
