#!/bin/sh

# Install SDKMan, the manager for Java development SDKs and tools.
install_sdkman() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"

  # Install SDKMAN if we don't already have it installed.
  log_info "Installing sdkman"
  __install_tool_from_url_and_script "sdk" "${sdkmanHome}" \
    "https://get.sdkman.io"
}

# Install jenv, the Java environment manager. It is a similar tool to pyenv.
install_jenv() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  log_info "Installing JENV"
  __install_tool_from_git "jenv" "${jenvHome}" \
    "https://github.com/jenv/jenv.git"
}

# Enable jenv in the current shell.
initialize_jenv_for_shell() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  if ! __tool_installed "jenv" "${jenvHome}"; then
    install_jenv
  fi

  # Make sure jenv is available on $PATH.
  if [ ! -x "$(command -v jenv)" ]; then
    log_info "Adding jenv binary path to \$PATH"
    export PATH="${jenvHome}/bin:${PATH}"
  fi

  log_info "Initializing jenv for shell"
  eval "$(jenv init -)"
}

# Make SDKMan shell functions available.
initialize_sdkman_for_shell() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"
  local sdkmanInitScript="${sdkmanHome}/bin/sdkman-init.sh"

  # Make sure sdkman is installed.
  if ! __tool_installed "sdk" "${sdkmanHome}"; then
    install_sdkman
  fi

  if [ -s "${sdkmanInitScript}" ]; then
    log_info "Initializing SDKMAN for shell"
    . "${sdkmanInitScript}"
  else
    warn "No SDKMAN initialization script found at: ${sdkmanInitScript}"
  fi
}

# Source Java components in a profile initialization file, such as .bashrc
# or .zshrc.
src_java_for_profile() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"

  # Use jenv to manage installed java versions.
  initialize_jenv_for_shell

  # THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
  export SDKMAN_DIR="${sdkmanHome}"
  initialize_sdkman_for_shell
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
