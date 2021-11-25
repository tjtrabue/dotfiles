#!/bin/sh

# Initialize KDE Plasma with my default configuration.
init_kde() {
  install_kwin_tiling
}

install_kwin_tiling() {
  __clone_kwin_tiling_repo
  __install_kwin_tiling_script
}

__clone_kwin_tiling_repo() {
  local repoUrl="https://github.com/kwin-scripts/kwin-tiling.git"
  local destDir="${WS}/$(basename "${repoUrl%.git}")"

  log_info "Cloning KWin Tiling repository to: ${BLUE}${destDir}${NC}"
  git clone "${repoUrl}" "${destDir}"
}

__install_kwin_tiling_script() {
  local kwinTilingDir="${WS}/kwin-tiling"

  (
    log_info "Installing KWin Tiling script"
    cd "${kwinTilingDir}"
    plasmapkg2 --type kwinscript -i .
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
