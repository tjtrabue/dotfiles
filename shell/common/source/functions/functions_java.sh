#!/bin/sh

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

# Enable jenv in the current shell.
initialize_jenv_for_shell() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  if ! tool_installed "jenv" "${jenvHome}"; then
    install_or_update_jenv
  fi

  # Make sure jenv is available on $PATH.
  if [ ! -x "$(command -v jenv)" ]; then
    log_info 'Adding jenv binary path to $PATH'
    export PATH="${jenvHome}/bin:${PATH}"
  fi

  log_info "Initializing jenv for shell: $(currentshell)"
  eval "$(jenv init -)"
}

# Make SDKMan shell functions available.
initialize_sdkman_for_shell() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"
  local sdkmanInitScript="${sdkmanHome}/bin/sdkman-init.sh"

  # Make sure sdkman is installed.
  if ! tool_installed "sdk" "${sdkmanHome}"; then
    install_or_update_sdkman
  fi

  if [ -s "${sdkmanInitScript}" ]; then
    log_info "Initializing SDKMAN for shell: $(currentshell)"
    . "${sdkmanInitScript}"
  else
    warn "No SDKMAN initialization script found at: ${sdkmanInitScript}"
  fi
}

# Install SDKMAN, the manager for Java SDKs and development tools.
# Updates SDKMAN if it is already installed.
install_or_update_sdkman() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"

  if [ -n "$(command -v sdk)" ]; then
    log_info "Updating sdkman"
    echo "y" | sdk upgrade
  else
    # Install SDKMAN if we don't already have it installed.
    log_info "Installing sdkman"
    install_tool_from_url_and_script "sdk" "${sdkmanHome}" \
      "https://get.sdkman.io"

    # Merge the default configuration file for sdkman with the config file we have
    # checked in to this repository.
    merge_sdkman_config
  fi
}

# Install jenv, the Java environment manager. It is a similar tool to pyenv. If
# jenv is already installed, this updates it to the latest stable version.
install_or_update_jenv() {
  local jenvHome="${JENV_HOME:-${HOME}/.jenv}"

  if [ -d "${jenvHome}" ]; then
    log_info "Updating JENV"
    git -C "${jenvHome}" reset --hard
    git -C "${jenvHome}" pull
  else
    log_info "Installing JENV"
    install_tool_from_git "jenv" "${jenvHome}" \
      "https://github.com/jenv/jenv.git"
  fi
}

# Combine the SDKMAN configuration file in the ~/.sdkman directory with the
# checked-in configuration file in this repository.
merge_sdkman_config() {
  local dotfilesSdkConfig="${DOTFILES_LINK}/sdkman/config"
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"
  local homeSdkConfig="${sdkmanHome}/etc/config"
  local sdkMergedTmp="$(mktemp -u /tmp/sdkman-merged-config-XXXXXXXXXX)"

  if [ ! -f "${homeSdkConfig}" ] && [ ! -h "${homeSdkConfig}" ]; then
    err "No SDKMAN configuration file found at: ${homeSdkConfig}"
    return 1
  fi

  # It's important to list the dotfiles sdkman config file BEFORE the
  # ~/.sdkman/etc/config file because the lines of the first file will take
  # precedence over those of succeeding files.
  cat "${dotfilesSdkConfig}" "${homeSdkConfig}" | rmduplines >"${sdkMergedTmp}"

  # Replace the contents of the ~/.sdkman/etc/config file with those of the temp
  # file.
  mv "${sdkMergedTmp}" "${homeSdkConfig}"

  # Delete the temp file.
  rm -f "${sdkMergedTmp}"
}

# Install JVM-related software with SDKMAN.
install_sdkman_packages() {
  local sdkmanHome="${SDKMAN_DIR:-${HOME}/.sdkman}"
  local sdkmanInitScript="${sdkmanHome}/bin/sdkman-init.sh"

  if [ -z "$(command -v sdk)" ]; then
    err "sdk function not found in current shell session. Make sure to source" \
      "the SDKMAN script at: ${GREEN}${sdkmanInitScript}${NC} and try again."
    return 1
  fi

  log_info "Installing SDKMAN packages"
  # Install latest GraalVM JDK.
  sdk install java "$(sdk list java | grep 'GraalVM' | awk '{print $NF}')"
  # Install default Groovy version.
  sdk install groovy
  # Maven is the most popular Java build and dependency management tool.
  sdk install maven
  # Gradle is a newer alternative to Maven built on Groovy.
  sdk install gradle
  # Micronaut is a framework for microservice development in Java. This is its
  # accompanying CLI tool.
  sdk install micronaut
  # Install leiningen for managing Clojure projects.
  sdk install leiningen

  # Make sure we register new Java versions with jenv
  add_sdkman_java_candidates_to_jenv
}

# Integrate installed Java SDKs into jenv by registering them.
add_sdkman_java_candidates_to_jenv() {
  local sdkJavaCandidatesDir="${SDKMAN_DIR:-${HOME}/.sdkman}/candidates/java"
  local dir
  local d

  if [ ! -d "${sdkJavaCandidatesDir}" ]; then
    err "Java candidates dir ${BLUE}${sdkJavaCandidatesDir}${NC} does not exist"
    return 1
  fi

  if [ -z "$(command -v jenv)" ]; then
    err "No jenv command found in current shell environment"
    return 2
  fi

  log_info "Adding SDKMAN java candidates to jenv"
  for dir in "${sdkJavaCandidatesDir}"/*; do
    # We want to make sure not to add the "current" symlink used by SDKMAN to
    # jenv.
    if [ -d "${dir}" ] && [ ! -h "${dir}" ]; then
      d="$(basename "${dir}")"
      log_info "Adding java candidate ${GREEN}${d}${NC} to jenv"
      jenv add "${dir}"
    fi
  done
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
