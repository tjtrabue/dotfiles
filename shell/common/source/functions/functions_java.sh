#!/bin/sh

# Install SDKMan, the manager for Java development SDKs and tools.
install_sdkman() {
  log_info "Installing sdkman"
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"

  # Install SDKMAN if we don't already have it installed.
  if ! __tool_installed "sdk" "${sdkmanHome}"; then
    curl -s "https://get.sdkman.io" | bash
  else
    warn "SDKMAN directory already found at ${sdkmanHome}"
  fi
}

# Install jenv, the Java environment manager. It is a similar tool to pyenv.
install_jenv() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  if ! __tool_installed "jenv" "${jenvHome}"; then
    git clone "https://github.com/jenv/jenv.git" "${jenvHome}"
  fi
}

# Enable jenv in the current shell.
initialize_jenv_for_shell() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  if ! __tool_installed "jenv" "${jenvHome}"; then
    install_jenv
  fi

  # Make sure jenv is available on $PATH.
  if [ ! -x "$(command -v jenv)" ]; then
    export PATH="${jenvHome}/bin:${PATH}"
  fi

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
    . "${sdkmanInitScript}"
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
