#!/bin/sh

# Install maildir utils (mu) and accompanying tools, such as mu4e, which
# provides Emacs integration with mu. It is best to install mu from source
# since the compiled versions in most package repositories tend to lag behind.
#
# Installation requires ninja, meson, a C
# compiler, and probably many other dependencies.
install_mu() {
  local gitRepoUrl="git@github.com:djcb/mu.git"
  local muInstallDir="${WS:-${HOME}/workspace}/mu"

  if [ -d "${muInstallDir}" ]; then
    log_info "Updating mu repository at: ${BLUE}${muInstallDir}${NC}"
    git -C "${muInstallDir}" clean -df
    git -C "${muInstallDir}" restore --staged .
    git -C "${muInstallDir}" restore .
    git -C "${muInstallDir}" pull
  else
    log_info "Cloning mu repository to: ${BLUE}${muInstallDir}${NC}"
    git clone "${gitRepoUrl}" "${muInstallDir}"
  fi

  if [ ! -d "${muInstallDir}" ]; then
    err "Could not install mu using Git"
    return 1
  fi

  (
    log_info "Compiling and installing mu"
    cd "${muInstallDir}" &&
      ./autogen.sh &&
      make &&
      sudo make install
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1