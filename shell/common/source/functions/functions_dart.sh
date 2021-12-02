#!/bin/sh

# Pull Dart configuration into the current shell profile.
src_dart_for_profile() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"

  if [ ! -d "${dvmRoot}" ]; then
    install_or_update_dvm
  fi

  __src_dvm_for_profile
}

# Install or update the dart version manager (dvm).
install_or_update_dvm() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"

  if [ -d "${dvmRoot}" ]; then
    __update_dvm
  else
    __install_dvm
  fi
}

install_latest_dart() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"

  if [ ! -d "${dvmRoot}" ]; then
    __install_dvm
  fi

  __src_dvm_for_profile
  dvm install latest --default
  __add_latest_dart_to_path
}

__add_latest_dart_to_path() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"
  local dvmDefaultEnv="${dvmRoot}/environments/default"
  local latestDartVersion

  latestDartVersion="$(grep 'PATH' "${dvmDefaultEnv}" |
    grep -E -o '[0-9]+\.[0-9]+\.[0-9]+')"

  if ! dvm list | grep -q -o "^${latestDartVersion}\$"; then
    err "Could not detect dart version ${latestDartVersion} installed by dvm"
    return 1
  fi

  log_info "Adding dart version ${latestDartVersion} to PATH"
  atp "${dvmRoot}/darts/${latestDartVersion}/bin"
}

__install_dvm() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"

  log_info "Installing DVM"
  install_tool_from_git "dvm" "${dvmRoot}" \
    "https://github.com/cbracken/dvm.git"
}

__update_dvm() {
  dvm upgrade
}

# Use DVM in the current shell.
__src_dvm_for_profile() {
  local dvmRoot="${DVM_ROOT:-${HOME}/.dvm}"
  local dvmScript="${dvmRoot}/scripts/dvm"

  if [ -f "${dvmScript}" ]; then
    . "${dvmScript}"
  else
    warn "DVM shell script not found at: ${BLUE}${dvmScript}${BLUE}"
  fi
}

# Install the Flutter SDK to a predictable location on the file system.
install_or_update_flutter_sdk() {
  local gitUrl="https://github.com/flutter/flutter.git"
  local flutterSdkBranch="stable"
  local destDir="${WS:-${HOME}/workspace}/$(basename "${gitUrl%.git}")"

  if [ -d "${destDir}" ]; then
    log_info "Updating Flutter SDK"
    git -C "${destDir}" reset --hard
    git -C "${destDir}" clean -df
    git -C "${destDir}" checkout "${flutterSdkBranch}"
    git -C "${destDir}" pull
  else
    log_info "Installing Flutter SDK to: ${BLUE}${destDir}${NC}"
    git clone -b "${flutterSdkBranch}" "${gitUrl}" "${destDir}"
  fi

  # Make sure Flutter's binaries are on PATH.
  atp "${destDir}/bin"

  # Download additional dependencies for Flutter and print diagnostic output.
  "${destDir}/bin/flutter" doctor
}

# Flutter sometimes needs extra dependencies that `flutter doctor` cannot
# automatically install. This function tries to take care of this shortcoming.
install_flutter_deps() {
  local os="$(getdistro)"

  log_info "Looking for extra dependencies needed by Flutter"
  case "${os}" in
  "Darwin")
    __install_flutter_deps_mac
    ;;
  esac
}

# Need a few extra dependencies on macOS that `flutter doctor` does not provide.
__install_flutter_deps_mac() {
  log_info "Installing Flutter dependencies for macOS"
  gem install cocoapods
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
