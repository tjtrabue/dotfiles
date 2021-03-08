#!/bin/sh

# Make ready the shell's prompt.
# This function should work for any shell, not just bash.
src_prompt_for_profile() {
  log_info "Initializing user prompt"
  src_starship_prompt_for_profile
}

# Install the cross-shell starship prompt.
install_starship() {
  local starshipUrl="https://starship.rs/install.sh"

  if [ ! -x "$(command -v starship)" ]; then
    log_info "Installing starship Prompt"
    curl -fsSL "${starshipUrl}" | bash
  else
    warn "starship command already found on path!"
  fi
}

# Get starship prompt ready for whichever shell we are using.
src_starship_prompt_for_profile() {
  local currentShell="$(currentshell)"

  if [ -z "${currentShell}" ]; then
    err "Could not determine current shell!"
    return 1
  fi

  if [ ! -x "$(command -v starship)" ]; then
    install_starship
  fi

  __activate_starship_for_shell
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
  __activate_starship 'eval "$(starship init bash)"' 'STARSHIP_ACTIVE_BASH'
}

__activate_starship_for_zsh() {
  __activate_starship 'eval "$(starship init zsh)"' 'STARSHIP_ACTIVE_ZSH'
}

__activate_starship_for_fish() {
  __activate_starship 'starship init fish | source' 'STARSHIP_ACTIVE_FISH'
}

# Generic activation function for starship prompt in the current shell. We do
# not want to re-activate starship after the first time because doing so can
# incur some strange side-effects in the shell's prompt.
__activate_starship() {
  local starshipInitCmd="$1"
  local shellControlEnvVar="$2"

  log_debug "Starhship activation command: ${starshipInitCmd}"
  log_debug "Starship control environment variable: ${shellControlEnvVar}"
  eval "${starshipInitCmd}"
  eval "export ${shellControlEnvVar}=1"
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
