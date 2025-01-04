#!/bin/sh

# Initialize pay-respects (a modern successor to thefuck) for the user's shell.
# See - https://codeberg.org/iff/pay-respects
src_pay_respects_for_profile() {
  if [ -x "$(command -v pay-respects)" ]; then
    case "$(currentshell)" in
    "bash")
      __src_pay_respects_for_shell "bash"
      ;;
    "zsh")
      __src_pay_respects_for_shell "zsh"
      ;;
    *)
      warn "Could not source pay-respects for shell: $1"
      ;;
    esac
  fi
}

__src_pay_respects_for_shell() {
  local userShell="${1}"
  local prAlias="f"

  if [ -z "${userShell}" ]; then
    err "No shell name provided"
    return 1
  fi

  log_info "Initializing ${CYAN}pay-respects${NC} for" \
    "${MAGENTA}${userShell}${NC} with alias: ${GREEN}${prAlias}${NC}"
  eval "$(pay-respects "${userShell}" --alias "${prAlias}")"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1