#!/bin/sh

# Install or update the fantastic and powerful swift-format tool.
# Requires a full swift installation.
install_or_update_swift_format() {
  local swiftFormatDir="${WS}/swift-format"

  if ! __swift_installed; then
    err "swift executable not found"
    return 1
  fi

  if [ -d "${swiftFormatDir}" ]; then
    __update_swift_format "${swiftFormatDir}"
  else
    __install_swift_format "${swiftFormatDir}"
  fi
}

# Print the semantic version of our Swift installation.
get_swift_version() {
  swift -version 2>&1 | grep -i -o 'swift version [0-9]\.[0-9]\.[0-9]' |
    awk '{print $4}'
}

__swift_installed() {
  test -x "$(command -v swift)"
}

__update_swift_format() {
  local swiftFormatDir="${1:-${WS}/swift-format}"

  git -C "${swiftFormatDir}" reset --hard
  git -C "${swiftFormatDir}" fetch
}

__install_swift_format() {
  local swiftFormatDir="${1:-${WS}/swift-format}"
  local swiftFormatGitUrl="https://github.com/apple/swift-format.git"
  local versionRef="swift-5.5-branch"

  __clone_swift_format "${swiftFormatDir}"
  __build_swift_format "${swiftFormatDir}"
}

__clone_swift_format() {
  local destDir="${1:-${WS}/swift-format}"
  local swiftFormatGitUrl="https://github.com/apple/swift-format.git"

  if [ ! -d "${destDir}" ]; then
    log_info "Cloning swift-format GitHub repository"
    git clone "${swiftFormatGitUrl}" "${destDir}"
  else
    warn "swift-format directory already exists at: ${BLUE}${destDir}${NC}"
  fi
}

# Compile and install the swift-format tool.
__build_swift_format() {
  local swiftFormatDir="${1:-${WS}/swift-format}"
  local swiftFormatReleaseDir="${swiftFormatDir}/.build/release"
  local installPrefix="/usr/local"
  local installDir="${installPrefix}/bin"

  mkdir -p "${installDir}"

  log_info "Installing swift-format from source"
  (
    cd "${swiftFormatDir}" &&
      __checkout_correct_swift_format_ref "${swiftFormatDir}" &&
      swift build -c release &&
      install -m 755 -t "${installDir}" "${swiftFormatReleaseDir}/swift-format"
  )
}

# We need to build the correct version of swift-format for our current Swift
# installation.
__checkout_correct_swift_format_ref() {
  local swiftFormatDir="${1:-${WS}/swift-format}"
  local swiftMajorMinorVersion
  local versionRef

  swiftMajorMinorVersion="$(get_swift_version | grep -o '^[0-9]\.[0-9]')"
  versionRef="$(git -C "${swiftFormatDir}" \
    branch -r --color=never --format="%(refname:lstrip=-1)" |
    grep "${swiftMajorMinorVersion}")"

  if [ -n "${versionRef}" ]; then
    log_info "Checking out swift-format ref: ${MAGENTA}${versionRef}${NC}"
    git -C "${swiftFormatDir}" checkout "${versionRef}"
  else
    err "Could not determine correct swift-format version ref"
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
