#!/bin/sh

# Activate broot to help navigate directories easily.
src_broot_for_profile() {
  # Broot works for bash, zsh, and fish. Possibly more. See `man broot' for more
  # details.
  local currentShell="$(currentshell)"

  # Error conditions
  __check_broot_installed || return 1
  __check_broot_supports_shell "${currentShell}" || return 2

  log_info "Activating 'br' function for the broot directory navigation tool."
  . <(broot --print-shell-function "${currentShell}")
}

__check_broot_installed() {
  # Error conditions
  if [ ! -x "$(command -v broot)" ]; then
    err "broot not installed. View online documentation for help."
    return 1
  fi
}

__check_broot_supports_shell() {
  local currentShell="${1}"

  if [ -z "${currentShell}" ]; then
    currentShell="$(currentshell)"
  fi

  if ! broot --print-shell-function "${currentShell}" >>/dev/null 2>&1; then
    err "broot does not appear to support shell: ${BLUE}${currentShell}${NC}." \
      "See 'man broot' for more details."
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
