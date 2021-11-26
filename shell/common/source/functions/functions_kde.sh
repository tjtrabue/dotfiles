#!/bin/sh

# Initialize KDE Plasma with my default configuration.
init_kde() {
  install_kwin_tiling
}

# Install the KWin Tiling plasmoid script to provide tiling window manager
# suppport for Plasma.
install_kwin_tiling() {
  __clone_or_update_kwin_tiling_repo
  __install_kwin_tiling_script
}

__clone_or_update_kwin_tiling_repo() {
  local repoUrl="https://github.com/kwin-scripts/kwin-tiling.git"
  local destDir="${WS}/$(basename "${repoUrl%.git}")"

  if [ -d "${destDir}" ]; then
    log_info "Updating KWin Tiling repo"
    git -C "${destDir}" clean -df
    git -C "${destDir}" reset --hard
    git -C "${destDir}" checkout "$(defaultbranch "${destDir}")"
    git -C "${destDir}" pull
  else
    log_info "Cloning KWin Tiling repository to: ${BLUE}${destDir}${NC}"
    git clone "${repoUrl}" "${destDir}"
  fi
}

__install_kwin_tiling_script() {
  local kwinTilingDir="${WS}/kwin-tiling"

  if [ -d "${kwinTilingDir}" ]; then
    (
      log_info "Installing KWin Tiling script"
      cd "${kwinTilingDir}"
      plasmapkg2 --type kwinscript -i .
    )
  else
    err "No KWin Tiling repository found at: ${BLUE}${kwinTilingDir}${NC}"
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
