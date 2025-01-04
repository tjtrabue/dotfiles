#!/bin/sh

# Initialize pay-respects (a modern successor to thefuck) for the user's shell.
# See - https://codeberg.org/iff/pay-respects
src_pay_respects_for_profile() {
  if [ -x "$(command -v pay-respects)" ]; then
    case "$(currentshell)" in
    "bash")
      __src_pay_respects_bash
      ;;
    "zsh")
      __src_pay_respects_zsh
      ;;
    *)
      warn "Could not source pay-respects for shell: $1"
      ;;
    esac
  fi
}

__src_pay_respects_bash() {
  local prAlias="f"

  log_info "Initializing ${CYAN}pay-respects${NC} for Bash with alias: ${prAlias}"
  eval "$(pay-respects bash --alias "${prAlias}")"
}

__src_pay_respects_zsh() {
  local prAlias="f"

  log_info "Initializing ${CYAN}pay-respects${NC} for Zsh with alias: ${prAlias}"
  eval "$(pay-respects zsh --alias "${prAlias}")"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1