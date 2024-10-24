#!/bin/sh

# Prepare Node.js environment for a shell.
# This function should be placed toward the end of your profile file, such as
# ~/.bashrc, or ~/.zshrc.
src_node_for_profile() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  # Install nvm if we do not yet have it installed.
  if ! tool_installed "nvm" "${nvmDir}"; then
    install_or_update_nvm
    install_latest_node
  fi

  if [ -d "${nvmDir}" ]; then
    if [ -s "${nvmDir}/nvm.sh" ]; then
      . "${nvmDir}/nvm.sh"
    fi
    if [ -s "${nvmDir}/bash_completion" ]; then
      . "${nvmDir}/bash_completion"
    fi
  fi
}

# Either install or update node version manager (nvm).
install_or_update_nvm() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  if [ -d "${nvmDir}" ]; then
    __update_nvm
  else
    __install_nvm
  fi

  __link_nvm_default_packages_file
}

# Use nvm to install the latest LTS version of Node.js.
install_latest_node() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"
  local pathFile="${PATH_FILE:-${HOME}/.path}"

  if ! tool_installed "nvm" "${nvmDir}"; then
    install_or_update_nvm
    . "${nvmDir}/nvm.sh"
  elif [ "$(command -v nvm)" = "" ]; then
    # If we have the nvm directory, just source the nvm script.
    . "${nvmDir}/nvm.sh"
  fi

  log_info "Installing latest Node.js version with nvm"

  # Install latest Node.js and set it as the currently used version.
  # Also set latest node as default and install latest npm, as well.
  nvm install --default --latest-npm --lts &&
    log_info "Writing path to latest node bin dir to: ${BLUE}${pathFile}${NC}" &&
    export_nvm_default_node_path
}

# Install global NPM packages for the currently selected Node.js version.
install_node_packages() {
  eval "npm install -g --force $(tr '\n' ' ' <"${NODE_PACKAGES_FILE}")"
}

# Update all globally installed NPM packages to the latest version.
update_node_packages() {
  if [ -z "$(command -v npm)" ]; then
    err "No npm executable found on PATH"
    return 1
  fi
  log_info "Updating Node version ${GREEN}$(node --version)${NC} packages"
  # Update NPM itself.
  npm install -g npm
  # Install all globally installed packages.
  npx npm-check --global --update-all
}

# Writes the default node version installed with nvm to the ~/.path file.
export_nvm_default_node_path() {
  local pathFile="${PATH_FILE:-${HOME}/.path}"
  local defaultNodeVersion
  local defaultNodeBinPath

  defaultNodeVersion="$(nvm version default 2>/dev/null)"

  if [ -z "${defaultNodeVersion}" ]; then
    err "Could not determine default node version with nvm"
    return 1
  fi

  defaultNodeBinPath="\${HOME}/.nvm/versions/node/${defaultNodeVersion}/bin"

  log_info "Writing default node path: ${BLUE}${defaultNodeBinPath}${NC} to" \
    "path file: ${BLUE}${pathFile}${NC}"

  if grep -q '.*\.nvm.*versions.*bin' "${pathFile}"; then
    sed -E -i "s|.*\.nvm.*versions.*bin/?\$|${defaultNodeBinPath}|" \
      "${pathFile}"
  else
    printf "\n%s\n" "${defaultNodeBinPath}" >>"${pathFile}"
    rmblanklines "${pathFile}"
  fi

  export_path "${pathFile}" "PATH"
}

# Install Node Version Manager (nvm)
__install_nvm() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  if [ ! -d "${nvmDir}" ]; then
    (
      log_info "Installing nvm"
      git clone "https://github.com/nvm-sh/nvm.git" "${nvmDir}"
      cd "${nvmDir}"
      git checkout "$(git describe --abbrev=0 --tags --match "v[0-9]*" \
        "$(git rev-list --tags --max-count=1)")"
    ) && \. "${nvmDir}/nvm.sh" &&
      log_info "Done!"
  else
    warn "${nvmDir} already exists"
  fi
}

# Update an existing nvm installation via git.
__update_nvm() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  if [ -d "${nvmDir}" ]; then
    log_info "Updating nvm"
    (
      cd "${nvmDir}"
      git fetch --tags origin
      git checkout $(git describe --abbrev=0 --tags --match "v[0-9]*" \
        $(git rev-list --tags --max-count=1))
    ) && \. "${nvmDir}/nvm.sh"
  else
    err "nvm directory not found at: ${BLUE}${nvmDir}${NC}"
    return 1
  fi
}

# nvm supports a ~/.nvm/default-packages file containing a list of global
# packages that will be installed with each new version of Node.js nvm installs.
# I keep my global Node.js packages in a custom text file, but its syntax is
# exactly the same as ~/.nvm/default-packages, so all we need to do is make a
# symlink and we're good!
__link_nvm_default_packages_file() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"
  local nvmDefaultPackagesFile="${nvmDir}/default-packages"
  local nodePackagesFile="${NODE_PACKAGES_FILE}"

  if [ ! -f "${nodePackagesFile}" ]; then
    err "No node packages file at: ${BLUE}${nodePackagesFile}${NC}"
    return 1
  fi

  log_info "Linking ${BLUE}${nodePackagesFile}${NC} to" \
    "${CYAN}${nvmDefaultPackagesFile}${NC}"
  ln -sf "${nodePackagesFile}" "${nvmDefaultPackagesFile}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1:foldlevel=0
