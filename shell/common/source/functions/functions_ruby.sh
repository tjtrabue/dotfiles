#!/bin/sh

install_ruby_packages() {
  local line

  while IFS='' read -r line || [ -n "${line}" ]; do
    gem install "${line}"
  done <"${RUBY_PACKAGES_FILE}"

  add_gem_bin_dirs_to_path
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
  # Download RVM, and do not add boilerplate to shell RC files.
  command curl -sSL "https://get.rvm.io" | bash -s -- --ignore-dotfiles
}

# Use rvm to install the latest ruby.
install_latest_ruby() {
  local rvmHome="${RVM_DIR:-${HOME}/.rvm}"
  local rvmScriptsDir="${rvmHome}/scripts"

  if ! tool_installed "rvm" "${rvmHome}"; then
    # Install rvm if necessary.
    install_rvm
    . "${rvmScriptsDir}/rvm"
  elif [ ! -x "$(command -v rvm)" ]; then
    # If we have the rvm directory, just source the rvm script.
    . "${rvmScriptsDir}/rvm"
  fi

  rvm install ruby --latest
  # Set the latest version of Ruby as default and switch to it.
  rvm alias create default ruby --latest
  rvm use default
}

# Update all Ruby Gems.
update_ruby_packages() {
  if [ -z "$(command -v gem)" ]; then
    err "gem command not found on PATH. Is Ruby installed?"
    return 1
  fi
  log_info "Updating Ruby version" \
    "${GREEN}$(ruby --version | awk '{print $2}')${NC} packages"
  gem update
}

# Prepare ruby environment for shell.
src_ruby_for_profile() {
  local rvmHome="${RVM_DIR:-${HOME}/.rvm}"

  # Install rvm if we do not yet have it installed.
  if ! tool_installed "rvm" "${rvmHome}"; then
    install_rvm
    install_latest_ruby
  fi

  # Load Ruby Version Manager (rvm) if available.
  if [ -s "${rvmHome}/scripts/rvm" ]; then
    . "${rvmHome}/scripts/rvm"
    rvm use default >>/dev/null 2>&1
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
