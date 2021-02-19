#!/bin/sh


# Install Node Version Manager (nvm)
install_nvm() {
  local nvmDir="${HOME}/.nvm"

  log_info "Installing nvm"
  if [ ! -d "${nvmDir}" ]; then
    (
      git clone "https://github.com/nvm-sh/nvm.git" "${nvmDir}"
      cd "${nvmDir}"
      git checkout "$(git describe --abbrev=0 --tags --match "v[0-9]*" \
        "$(git rev-list --tags --max-count=1)")"
    ) && \. "${nvmDir}/nvm.sh"
    log_info "Done!"
  else
    warn "${nvmDir} already exists"
  fi
}

# Use nvm to install the latest version of Node.js.
install_latest_node() {
  if ! __nvm_installed; then
    install_nvm
  fi
  # Install latest Node.js and set it as the currently used version.

  nvm install node && nvm use node && nvm alias default node
}

install_node_packages() {
  if [ "$(command -v parallel)" != "" ]; then
    parallel --bar npm install -g :::: "$NODE_PACKAGES_FILE"
  else
    xargs npm install -g <"$NODE_PACKAGES_FILE"
  fi
}

update_node_packages() {
  # Uses the npm-check-updates tool to get packages marked for update
  local packagesToUpdate="$(ncu -gu 2>/dev/null | grep '^npm' | sed '/^$/d')"

  if [ -n "$packagesToUpdate" ]; then
    eval "npm install -g" "$packagesToUpdate"
  else
    log_info "No packages to update."
  fi
}

# Prepare Node.js environment for a shell.
# This function should be placed toward the end of your profile file, such as
# ~/.bashrc, or ~/.zshrc.
src_node_for_profile() {
  if [ -d "${NVM_DIR}" ]; then
    if [ -s "${NVM_DIR}/nvm.sh" ]; then
      . "${NVM_DIR}/nvm.sh"
    fi
    if [ -s "${NVM_DIR}/bash_completion" ]; then
      . "${NVM_DIR}/bash_completion"
    fi
  fi
}

# Return 0 if nvm is installed. Return non-zero otherwise.
__nvm_installed () {
  local nvmHome="${HOME}/.nvm"

  if [ "$(command -v nvm)" = "" ] && [ ! -d "${nvmHome}" ]; then
    return 1
  fi

  return 0
}

#vim foldenable:foldmethod=indent:foldnestmax=1
