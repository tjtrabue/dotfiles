#!/bin/sh

# Prepare fasd aliases for the current shell.
src_fasd_for_profile() {
  if [ ! -x "$(command -v fasd)" ]; then
    install_fasd
  fi
  __src_fasd
  __add_fasd_aliases
}

# Install fasd for the current user
install_fasd() {
  local ws="${WS:-${HOME}/workspace}"
  local fasdRepo="${ws}/fasd"
  # The directory containing the 'bin' directory for fasd.
  local fasdInstallPrefix="${HOME}"
  local fasdBinDir="${fasdInstallPrefix}/bin"

  if [ ! -x "${fasdBinDir}/fasd" ]; then
    __clone_fasd
    (
      log_info "Installing fasd"
      cd "${fasdRepo}"
      PREFIX="${fasdInstallPrefix}" make install
    )
  else
    warn "fasd already installed at: ${fasdBinDir}"
  fi

}

# Clone the fasd git repository
__clone_fasd() {
  local ws="${WS:-${HOME}/workspace}"
  local fasdRepo="${ws}/fasd"
  local fasdGitUrl="https://github.com/clvv/fasd.git"

  mkdir -p "${ws}"
  if [ ! -d "${fasdRepo}" ]; then
    log_info "Cloning fasd repository"
    git clone "${fasdGitUrl}" "${fasdRepo}"
  else
    warn "fasd repo already present at: ${fasdRepo}"
  fi
}

# Activate fasd for the current shell.
__src_fasd() {
  local fasdCache="${HOME}/.fasd-init-cache"
  __create_fasd_startup_cache
  log_info "Activating fasd for shell"
  . "${fasdCache}"
}

# Create a fasd cache in order to speed up shell startup.
__create_fasd_startup_cache() {
  local fasdCache="${HOME}/.fasd-init-cache"
  if [ "$(command -v fasd)" -nt "${fasdCache}" -o ! -s "${fasdCache}" ]; then
    log_info "Creating fasd startup cache to enhance performance"
    fasd --init auto >|"${fasdCache}"
  fi
}

# Add custom fasd aliases.
__add_fasd_aliases() {
  log_info "Sourcing custom fasd aliases"
  # Edit files with the established editor program.
  alias v='f -e $(eval echo \${EDITOR})'
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
