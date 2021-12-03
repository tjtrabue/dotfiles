#!/bin/sh

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

# Install a given Android image for development. If no image path is provided,
# use the latest compatible Android image.
install_android_image_and_avd() {
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

# vim:foldenable:foldmethod=indent:foldnestmax=1
