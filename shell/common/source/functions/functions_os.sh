#!/bin/sh

# Gets the Linux OS's name and version.
getosinfo() {
  # Figure out the distro based on one of a few things.
  if [ -f "/etc/os-release" ]; then
    # freedesktop.org and systemd
    source "/etc/os-release"
    OS="${NAME}"
    VER="${VERSION_ID:-${BUILD_ID}}"
  elif type lsb_release >>/dev/null 2>&1; then
    # linuxbase.org
    OS="$(lsb_release -si)"
    VER="$(lsb_release -sr)"
  elif [ -f "/etc/lsb-release" ]; then
    # For some versions of Debian/Ubuntu without lsb_release command
    source "/etc/lsb-release"
    OS="${DISTRIB_ID}"
    VER="${DISTRIB_RELEASE}"
  elif [ -f "/etc/debian_version" ]; then
    # Older Debian/Ubuntu/etc.
    OS="Debian"
    VER="$(cat "/etc/debian_version")"
  elif [ -f "/etc/SuSe-release" ]; then
    # Older SuSE/etc.
    OS="SuSE"
    VER="unknown"
  elif [ -f "/etc/redhat-release" ]; then
    # Older Red Hat, CentOS, etc.
    OS="RedHat"
    VER="unknown"
  else
    OS="$(uname -s)"
    VER="$(uname -r)"
  fi

  # Return the distribution name and version.
  printf "%s: %s\n%s: %s\n" \
    "Distribution" "${OS}" \
    "Version" "${VER}"
}

# Determine which OS family is currently in use.
# Output can be one of:
#   Linux
#   Darwin
#   Cygwin
getostype() {
  local osType

  if [ -x "$(command -v uname)" ]; then
    osType="$(uname -s)"
  else
    err "Could not determine OS type"
    return 1
  fi

  echo "${osType}"
}

# Retrieves the system package manager for the current operating system.
getpm() {
  local distId
  local pm=""

  # Get thr distribution ID:
  distId="$(getdistro)"

  case "${distId}" in
    "Arch Linux")
      pm="pacman"
      ;;
    "CentOS" | "RedHat")
      pm="yum"
      ;;
    "Darwin")
      pm="brew"
      ;;
    "Ubuntu")
      pm="apt-get"
      ;;
    *)
      err "Unknown OS: ${BLUE}${distId}${NC}"
      return 1
      ;;
  esac

  # Return the name of the package manager:
  printf "%s\n" "${pm}"
}

# Retrieve the OS distribution name only
getdistro() {
  __get_os_field "Distribution"
}

# Retrieve the OS distribution version only
getosversion() {
  __get_os_field "Version"
}

# Get a field returned from the `getosinfo` function.
__get_os_field() {
  local field="${1}"

  if [ -z "${field}" ]; then
    err "No field name provided"
    return 1
  fi

  getosinfo | grep "${field}:" | sed -E "s/^\s*${field}:\s*//"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
