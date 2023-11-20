#!/bin/sh

# Install eza from source.
#
# Many Linux distros, including Ubuntu, have lame versions of eza in their
# package repositories. It's best to install it yourself.
install_or_update_eza() {
  local ezaGitUrl="https://github.com/eza-community/eza.git"
  local ezaDest="${WS}/eza"
  local ezaInstallPrefix="${HOME}/.local"
  local ezaInstallDir="${ezaInstallPrefix}/bin"

  mkdir -p "$(dirname "${ezaDest}")" "${ezaInstallDir}"
  clone_or_update_git_repo "${ezaGitUrl}" "${ezaDest}"
  (
    log_info "Installing eza" &&
      cd "${ezaDest}" &&
      cargo build --release &&
      cp "target/release/eza" "${ezaInstallDir}/"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1