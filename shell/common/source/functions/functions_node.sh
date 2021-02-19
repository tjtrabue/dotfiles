#!/bin/sh

# Install Node Version Manager (nvm)
install_nvm() {
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

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
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  if ! __tool_installed "nvm" "${nvmDir}"; then
    install_nvm
  fi
  # Install latest Node.js and set it as the currently used version.

  nvm install node && nvm use node && nvm alias default node
}

# Install global NPM packages for the currently selected Node.js version.
install_node_packages() {
  xargs npm install -g <"${NODE_PACKAGES_FILE}"
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
  local nvmDir="${NVM_DIR:-${HOME}/.nvm}"

  # Install nvm if we do not yet have it installed.
  if ! __tool_installed "nvm" "${nvmDir}"; then
    install_nvm
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

#vim foldenable:foldmethod=indent:foldnestmax=1
