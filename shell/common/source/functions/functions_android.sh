#!/bin/sh

# Install Android SDKs, images, and virtual devices, and set up environment
# variables for Android development.
prep_android_dev_environment() {
  local os="$(getdistro)"

  log_info "Preparing Android development environment"
  case "${os}" in
  "Arch Linux")
    __prep_android_dev_environment_arch
    ;;
  *)
    err "Could not setup Android development environment for os:" \
      "${GREEN}${os}${NC}"
    return 1
    ;;
  esac

  command cat <<EOF
Congrats! You're almost done setting up your Flutter development environment!

FINAL STEPS:
- Restart your system so that the privilages of your new user groups will take
  effect.
- Run these commands:
  - install_android_dev_software
EOF
}

# Install a given Android image for development. If no image path is provided,
# use the latest compatible Android image.
install_android_dev_software() {
  local androidImage="${1}"
  local latestImage
  local androidAvdName

  if [ -z "${androidImage}" ]; then
    log_info "Getting latest compatible Android image"
    latestImage="$(sdkmanager --list 2>/dev/null |
      grep -o "system-images;android-.*;default;x86_64" |
      tail -1)"

    if [ -n "${latestImage}" ]; then
      log_info "Image found: ${GREEN}${latestImage}${NC}"
    else
      err "Could not find compatible Android image automatically"
      return 1
    fi
  fi

  log_info "Installing Android commandline tools"
  sdkmanager --install "cmdline-tools;latest"

  androidAvdName="$(echo "${androidImage}" |
    sed -E 's/.*(android-[0-9]+).*/\1/')"

  log_info "Installing Android image: ${GREEN}${androidImage}${NC}"
  sdkmanager --install "${androidImage}"

  log_info "Creating AVD ${YELLOW}${androidAvdName}${NC} for image:" \
    "${GREEN}${androidImage}${NC}"
  avdmanager create avd \
    --name "${androidAvdName}" \
    --package "${androidImage}"
}

__prep_android_dev_environment_arch() {
  local androidSdk="/opt/android-sdk"

  log_info "Preparing Android development environment for Arch Linux"
  aurhi android-sdk android-sdk-platform-tools android-sdk-build-tools
  aurhi android-platform

  sudo groupadd android-sdk
  sudo gpasswd -a "${USER}" android-sdk
  sudo setfacl -R -m g:android-sdk:rwx "${androidSdk}"
  sudo setfacl -d -m g:android-sdk:rwX "${androidSdk}"

  # Make sure the ANDROID_SDK_ROOT is set properly before adding SDK paths to
  # ~/.path.
  export ANDROID_SDK_ROOT="${androidSdk}"
  add_android_sdk_to_path
}

# Add android SDK binary paths to ~/.path
add_android_sdk_to_path() {
  local pathFile="${PATH_FILE}"

  if [ ! -d "${ANDROID_SDK_ROOT}" ]; then
    err "Variable ANDROID_SDK_ROOT is not set to a directory"
    return 1
  fi

  log_info "Adding Android SDK paths to ${BLUE}${pathFile}${NC}"
  atp '\${ANDROID_SDK_ROOT}/platform-tools/'
  atp '\${ANDROID_SDK_ROOT}/tools/bin/'
  atp '\${ANDROID_SDK_ROOT}/emulator'
  atp '\${ANDROID_SDK_ROOT}/tools/'
}

# Dynamically create shell alaises for running installed Android emulators.
create_android_emulator_aliases() {
  local avdName
  local extraDotfiles="${EXTRA_DOTFILES:-${HOME}/.extra}"
  local emulatorAliasFile="${extraDotfiles}/emulator_aliases.sh"

  mkdir -p "${extraDotfiles}"

  if [ ! -x "$(command -v avdmanager)" ]; then
    warn 'Could not find avdmanager command on $PATH'
    return 0
  fi

  rm -f "${emulatorAliasFile}"

  for avdName in $(avdmanager list avd 2>/dev/null |
    grep "Name:" |
    awk '{print $2}'); do
    echo "alias run_${avdName}='emulator @${avdName} -no-boot-anim" \
      "-netdelay none -no-snapshot -wipe-data &'" >>"${emulatorAliasFile}"
  done
}

# Predicate function for determining whether all relevant Android command line
# developer tools are properly installed.
android_cmdline_tools_installed() {
  [ -x "$(command -v avdmanager)" ] && [ -x "$(command -v sdkmanager)" ]
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
