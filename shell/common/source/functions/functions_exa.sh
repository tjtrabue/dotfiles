#!/bin/sh

# Install exa from source.
#
# Many Linux distros, including Ubuntu, have lame versions of exa in their
# package repositories. It's best to install it yourself.
install_or_update_exa() {
  local exaGitUrl="https://github.com/ogham/exa.git"
  local exaDest="${WS}/exa"
  local exaInstallPrefix="${HOME}/.local"
  local exaInstallDir="${exaInstallPrefix}/bin"

  mkdir -p "$(dirname "${exaDest}")" "${exaInstallDir}"
  clone_or_update_git_repo "${exaGitUrl}" "${exaDest}"
  (
    log_info "Installing exa" && \
    cd "${exaDest}" &&
    cargo build --release &&
    cp "target/release/exa" "${exaInstallDir}/"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1