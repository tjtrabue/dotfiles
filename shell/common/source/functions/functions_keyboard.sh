#!/bin/sh

# Install the Wally keyboard flashing tool from ZSA Technologies.
# This tool works with any ZSA keyboards.
install_zsa_wally() {
  local wallyGitUrl="https://github.com/zsa/wally.git"
  local cloneParentDir="${WS:-${HOME}/workspace}"
  local wallyDestDir="${cloneParentDir}/$(basename "${wallyGitUrl%.git}")"
  local wallyBuildDir="${wallyDestDir}/build"
  local wallyArtifact="${wallyBuildDir}/wally"
  local installDir="/usr/local/bin"

  if ! __verify_wally_dependencies_present; then
    err "wally dependencies were not satisfied"
    return 1
  fi

  __clone_or_update_wally

  (
    cd "${wallyDestDir}" &&
    log_info "Adding go.sum entry to wally project for wails" &&
    go mod download github.com/wailsapp/wails &&
    log_info "Building wally executable" &&
    wails build &&
    log_info "Installing wally executable to: ${BLUE}${installDir}${NC}" &&
    install -m 755 -t "${installDir}" "${wallyArtifact}"
  )
}

__verify_wally_dependencies_present() {
  if [ ! -x "$(command -v wails)" ]; then
    err "${MAGENTA}wails${NC} executable needed to build wally"
    return 1
  fi
}

__clone_or_update_wally() {
  local wallyGitUrl="https://github.com/zsa/wally.git"
  local cloneParentDir="${WS:-${HOME}/workspace}"
  local wallyDestDir="${cloneParentDir}/$(basename "${wallyGitUrl%.git}")"

  if [ -d "${wallyDestDir}" ]; then
    log_info "Pulling updates for wally"
    git -C "${wallyDestDir}" reset --hard
    git -C "${wallyDestDir}" clean -df
    git -C "${wallyDestDir}" pull
  else
    log_info "Cloning wally keyboard flashing tool to:" \
      "${BLUE}${wallyDestDir}${NC}"
    git clone "${wallyGitUrl}" "${wallyDestDir}"
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
