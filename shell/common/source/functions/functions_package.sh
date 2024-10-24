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
    err "No install command given"
    return 1
  elif [ -z "${packageFile}" ]; then
    err "No package file given"
    return 2
  elif [ ! -f "${packageFile}" ] && [ ! -h "${packageFile}" ]; then
    err "Package file ${BLUE}${packageFile}${NC} does not exist"
    return 3
  fi

  log_info "Installing packages with command: ${GREEN}${installCmd}${NC}"
  while IFS="" read -r package || [ -n "${package}" ]; do
    # Ignore commented lines.
    if echo "${package}" | grep -v '^#.*' >>/dev/null 2>&1; then
      log_info "Installing package: ${BLUE}${package}${NC}"
      log_debug "Install command:" \
        "${GREEN}${installCmd}${NC} ${BLUE}${package}${NC}"
      eval "${installCmd} ${package}"
    fi
  done <"${packageFile}"
}

# Many useful CLI and system tools are installed through secondary programming
# environments, such as Perl's CPAN, Ruby's CRAN, Python PIP or Node.js's NPM
# archive. This function issues commands to update all of those packages.
update_auxiliary_packages() {
  log_info "Updating auxiliary packages"

  update_node_packages
  update_python_packages
  update_ruby_packages
  update_perl_packages
  update_texlive_packages
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
