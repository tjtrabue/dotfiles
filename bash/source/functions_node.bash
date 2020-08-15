#!/usr/bin/env bash

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

#vim foldenable:foldmethod=indent:foldnestmax=1
