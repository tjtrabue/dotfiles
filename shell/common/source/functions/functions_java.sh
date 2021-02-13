#!/bin/sh

# Install SDKMan, the manager for Java development SDKs and tools.
install_sdkman() {
  log_info "Installing sdkman"
  local sdkmanHome="${HOME}/.sdkman"

  # Install SDKMAN if we don't already have it installed.
  if ! declare -f sdk &>/dev/null && [ ! -d "${sdkmanHome}" ]; then
    curl -s "https://get.sdkman.io" | bash
  else
    warn "SDKMAN directory already found at ${sdkmanHome}"
  fi
}

# Make SDKMan shell functions available.
initialize_sdkman_for_shell() {
  local sdkmanHome="${HOME}/.sdkman"
  local sdkmanInitScript="${sdkmanHome}/bin/sdkman-init.sh"

  if [ -s "${sdkmanInitScript}" ]; then
    . "${sdkmanInitScript}"
  fi
}

# Source Java components in a profile initialization file, such as .bashrc
# or .zshrc.
src_java_for_profile() {
  # Use jenv to manage installed Java versions (if available)
  if [ -n "$(command -v jenv)" ]; then
    eval "$(jenv init -)"
  fi

  #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
  # Home directory for sdkman program.
  export SDKMAN_DIR="${HOME}/.sdkman"
  initialize_sdkman_for_shell
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
