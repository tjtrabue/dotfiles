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
  local rvmHome="${RVM_DIR:-${HOME}/.rvm}"

  install_tool_from_url_and_script "rvm" "${rvmHome}" \
    "https://get.rvm.io"
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
}

# Update all Ruby Gems.
update_ruby_packages() {
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
