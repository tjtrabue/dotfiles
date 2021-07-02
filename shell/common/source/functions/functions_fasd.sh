#!/bin/sh

# Prepare fasd aliases for the current shell.
src_fasd_for_profile() {
  if [ ! -x "$(command -v fasd)" ]; then
    install_fasd
  fi
  eval "$(fasd --init auto)"
}

# Install fasd for the current user
install_fasd() {
  local ws="${WS:-${HOME}/workspace}"
  local fasdRepo="${ws}/fasd"
  # The directory containing the 'bin' directory for fasd.
  local fasdInstallPrefix="${HOME}"

  if [ -x "${fasdInstallPrefix}/bin/fasd" ]; then
    err "fasd already installed at ${fasdInstallPrefix}/bin/"
    return 1
  fi

  __clone_fasd
  (
    cd "${fasdRepo}"
    PREFIX="${fasdInstallPrefix}" make install
  )
}

# Clone the fasd git repository
__clone_fasd() {
  local ws="${WS:-${HOME}/workspace}"
  local fasdRepo="${ws}/fasd"

  mkdir -p "${ws}"
  if [ ! -d "${fasdRepo}" ]; then
    git -C "${ws}" clone "https://github.com/clvv/fasd.git"
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
