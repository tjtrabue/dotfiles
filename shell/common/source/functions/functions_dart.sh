#!/bin/sh

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
