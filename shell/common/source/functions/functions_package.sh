#!/bin/sh

# A generic package installer function that operates via an install command
# string and a package file. This is a pattern used prolifically throughout this
# dotfiles project, so abstracting it to a higher order function made sense.
#
# $1 - The install command applied to each package in ${packageFile}.
# $2 - The package file containing a list of packages to install; one per line.
install_packages_from_file_with_tool() {
  local installCmd="${1}"
  local packageFile="${2}"
  local package

  if [ -z "${installCmd}" ]; then
    err "No install command given."
    return 1
  elif [ ! -f "${packageFile}" ] && [ ! -h "${packageFile}" ]; then
    err "Package file ${packageFile} does not exist."
    return 2
  fi

  log_info "Installing packages with command: ${GREEN}${installCmd}${NC}"
  while read -r package || [ -n "${package}" ]; do
    # Ignore commented lines.
    if echo "${package}" | grep -v '^#.*' >>/dev/null 2>&1; then
      log_info "Installing package: ${BLUE}${package}${NC}"
      log_debug "Install command:" \
        "${GREEN}${installCmd}${NC} ${BLUE}${package}${NC}"
      eval "${installCmd} ${package}"
    fi
  done <"${packageFile}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
