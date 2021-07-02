#!/bin/sh

# Activate broot to help navigate directories easily.
src_broot_for_profile() {
  local brootHome="${BROOT_HOME:-${HOME}/.config/broot}"
  local brootLauncher="${brootHome}/launcher/bash/br"

  if [ -x "$(command -v broot)" ] && [ -f "${brootLauncher}" ]; then
    log_info "Activating broot directory navigation tool."
    . "${brootLauncher}"
  else
    warn "broot not installed correctly. View online documentation for help."
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
