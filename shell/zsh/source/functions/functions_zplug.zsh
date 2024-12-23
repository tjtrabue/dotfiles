#!/usr/bin/env zsh

# Update the zplug repository, usually found at ~/.zplug.
update_zplug() {
  local zplugHome="${ZPLUG_HOME:-${HOME}/.zplug}"

  if [ -d "${zplugHome}" ]; then
    log_info "Updating zplug"
    git -C "${zplugHome}" restore --staged .
    git -C "${zplugHome}" restore .
    git -C "${zplugHome}" pull
  else
    warn "zplug repository not found at: ${BLUE}${zplugHome}${NC}"
  fi
}

# Update all plugins installed through zplug.
update_zplug_and_plugins() {
  if [ -n "$(command -v zplug)" ]; then
    log_info "Updating zplug and all plugins"
    update_zplug
    zplug update
  else
    warn "'zplug' function not sourced in this shell; cannot update plugins"
  fi
}