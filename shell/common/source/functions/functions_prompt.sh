#!/bin/sh

# Make ready the shell's prompt.
# This function should work for any shell, not just bash.
src_prompt_for_profile() {
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

  # Only source starship once. Sourcing the prompt multiple times can cause
  # odd side-effects in Zsh, especially in Zsh's vim-mode.
  if [ -z "${STARSHIP_SOURCED}" ] || [ "${STARSHIP_SOURCED}" -le 0 ]; then
    eval "$(starship init ${currentShell})"
    export STARSHIP_SOURCED=1
  fi
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
