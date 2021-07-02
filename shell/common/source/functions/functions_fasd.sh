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
  local fasdBinDir="${fasdInstallPrefix}/bin"

  if [ ! -x "${fasdBinDir}/fasd" ]; then
    __clone_fasd
    (
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
    git clone "${fasdGitUrl}" "${fasdRepo}"
  else
    warn "fasd repo already present at: ${fasdRepo}"
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
