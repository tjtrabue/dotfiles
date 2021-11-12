#!/bin/sh

# Install packages managed by the go-lang language.
install_go_packages() {
  local goPackagesFile="${GO_PACKAGES_FILE}"

  if [ ! -f "${goPackagesFile}" ]; then
    err "No Go URL file found"
    return 1
  fi

  if [ -x "$(command -v parallel)" ]; then
    parallel --bar go install :::: "${goPackagesFile}"
  else
    xargs go install <"${goPackagesFile}"
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
