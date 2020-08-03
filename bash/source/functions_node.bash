#!/usr/bin/env bash

install_npm_global_packages() {
  if [ "$(command -v parallel)" != "" ]; then
    parallel --bar npm install -g :::: "$NPM_GLOBAL_PACKAGES_FILE"
  else
    xargs npm install -g < "$NPM_GLOBAL_PACKAGES_FILE"
  fi
}

update_npm_global_packages() {
  # Uses the npm-check-updates tool to get packages marked for update
  local packagesToUpdate="$(ncu -gu 2> /dev/null | grep '^npm' | sed '/^$/d')"

  if [ -n "$packagesToUpdate" ]; then
    eval "npm install -g" "$packagesToUpdate"
  else
    log_info "No packages to update."
  fi
}

#vim foldenable:foldmethod=indent:foldnestmax=1
