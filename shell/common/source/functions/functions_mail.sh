#!/bin/sh

# Use Mutt/Neomutt depending on what's installed,
# and open in attachments directory if available.
mutt() {
  local muttCommand

  muttCommand="$(command -v mutt)"

  if [ "$(command -v neomutt)" != "" ]; then
    muttCommand="$(command -v neomutt)"
  fi

  if [ -z "$muttCommand" ]; then
    err 'Could not find mutt or neomutt executable on $PATH.'
    return 1
  fi

  if [ -d "$MUTT_ATTACHMENTS_DIR" ]; then
    pushd "$MUTT_ATTACHMENTS_DIR" &>/dev/null || return 2
    eval "$muttCommand"
    popd &>/dev/null || return 2
  else
    warn "No attachments directory found"
    eval "$muttCommand"
  fi
}

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
