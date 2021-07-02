#!/bin/sh

# Activate thefuck for the current shell with a specified alias. Typing
# 'thefuck' is hilarious, but come on. We're professionals.
src_thef_for_profile() {
  local thefAlias="${THEF_ALIAS:-oops}"

  __install_thef
  log_info "Activating thef*** with alias: ${thefAlias}"
  eval "$(thefuck --alias "${thefAlias}")"
}

__install_thef() {
  if [ ! -x "$(command -v thefuck)" ]; then
    log_info "Installing thef***"
    python3 -m pip install --user --upgrade thefuck
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
