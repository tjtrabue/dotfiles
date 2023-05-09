#!/bin/sh

# Make ready the shell's prompt.
# This function should work for any shell, not just bash.
src_prompt_for_profile() {
  log_info "Initializing user prompt"
  src_starship_prompt_for_profile
}

# Install the cross-shell starship prompt.
install_or_update_starship() {
  local starshipUrl="https://starship.rs/install.sh"

  log_info "Installing starship Prompt"
  curl -fsSL "${starshipUrl}" | sh
}

# Get starship prompt ready for whichever shell we are using.
src_starship_prompt_for_profile() {
  local currentShell="$(currentshell)"

  if [ -z "${currentShell}" ]; then
    err "Could not determine current shell!"
    return 1
  fi

  if [ ! -x "$(command -v starship)" ]; then
    install_or_update_starship
  fi

  # Only activate starship once for any given shell. If we try to activate it
  # more than once, we can experience some strange side-effects.
  if [ "${STARSHIP_PROMPT_ACTIVE}" != 1 ]; then
    __activate_starship_for_shell
    export STARSHIP_PROMPT_ACTIVE=1
  fi
}

# Conditionally determine how to activate Starship prompt for the current shell.
__activate_starship_for_shell() {
  local currentShell="$(currentshell)"

  log_debug "Activating starship prompt for ${currentShell}"
  case "${currentShell}" in
  "bash")
    __activate_starship_for_bash
    ;;
  "zsh")
    __activate_starship_for_zsh
    ;;
  "fish")
    __activate_starship_for_fish
    ;;
  *)
    warn "Could not determine how to activate Starship for ${currentShell}"
    ;;
  esac
}

__activate_starship_for_bash() {
  eval "$(starship init bash)"
}

__activate_starship_for_zsh() {
  eval "$(starship init zsh)"
}

__activate_starship_for_fish() {
  starship init fish | source
}

# Initialize the classic powerline shell prompt.
src_powerline_for_profile() {
  local currentShell="$(currentshell)"
  local powerlineBindings="$(__get_powerline_bindings_for_shell)"

  if [ "${currentShell}" = "bash" ]; then
    export POWERLINE_BASH_CONTINUATION=1
    export POWERLINE_BASH_SELECT=1
  fi

  if [ -f "${powerlineBindings}" ]; then
    . "${powerlineBindings}"
  fi

  powerline-daemon -q
}

# Get the path to the powerline bindings file for the current shell.
__get_powerline_bindings_for_shell() {
  local pythonLib="${HOME}/.local/lib"
  local baseRegex=".*python3.[0-9]+.*bindings"
  local currentShell="$(currentshell)"
  local bindingsRegex="powerline\.((ba)|(z))?sh"

  find "${pythonLib}" -type f \
    -regextype 'posix-extended' \
    -regex "${baseRegex}/${currentShell}/${bindingsRegex}" |
    sort -V |
    tail -1
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
