#!/bin/sh

# Bump a SemVer's major version component.
# i.e., 1.2.3 -> 2.0.0
bumpmajor() {
  local version="${1}"
  if ! __check_version_string "${version}"; then
    return 1
  fi
  local suffix="$(echo "${version}" | sed -E 's/[0-9]+\.[0-9]+\.[0-9]+(.*)/\1/')"
  local nonSnapshotVersion="$(printf '%s' "${version}" | sed 's/-.*$//')"
  local majorVersion="$(__parse_version_string_for_major "${nonSnapshotVersion}")"
  printf '%s.%s.%s%s' "$((majorVersion += 1))" "0" "0" "${suffix}"
}

# Bump a SemVer's minor version component.
# i.e., 1.2.3 -> 1.3.0
bumpminor() {
  local version="${1}"
  if ! __check_version_string "${version}"; then
    return 1
  fi
  local suffix="$(echo "${version}" | sed -E 's/[0-9]+\.[0-9]+\.[0-9]+(.*)/\1/')"
  local nonSnapshotVersion="$(printf '%s' "${version}" | sed 's/-.*$//')"
  local majorVersion="$(__parse_version_string_for_major "${nonSnapshotVersion}")"
  local minorVersion="$(__parse_version_string_for_minor "${nonSnapshotVersion}")"
  printf '%s' "${majorVersion}.$((minorVersion += 1)).0${suffix}"
}

# Bump a SemVer's patch version component.
# i.e., 1.2.3 -> 1.2.4
bumppatch() {
  local version="${1}"
  if ! __check_version_string "${version}"; then
    return 1
  fi
  local suffix="$(echo "${version}" | sed -E 's/[0-9]+\.[0-9]+\.[0-9]+(.*)/\1/')"
  local nonSnapshotVersion="$(printf '%s' "${version}" | sed 's/-.*$//')"
  local majorVersion="$(__parse_version_string_for_major "${nonSnapshotVersion}")"
  local minorVersion="$(__parse_version_string_for_minor "${nonSnapshotVersion}")"
  local patchVersion="$(__parse_version_string_for_patch "${nonSnapshotVersion}")"
  printf '%s' "${majorVersion}.${minorVersion}.$((patchVersion += 1))${suffix}"
}

__parse_version_string_for_major() {
  local version="${1}"
  printf '%s' "${version}" | sed -E 's/([0-9]+)\.[0-9]+\.[0-9]+/\1/'
}

__parse_version_string_for_minor() {
  local version="${1}"
  printf '%s' "${version}" | sed -E 's/[0-9]+\.([0-9]+)\.[0-9]+/\1/'
}

__parse_version_string_for_patch() {
  local version="${1}"
  printf '%s' "${version}" | sed -E 's/[0-9]+\.[0-9]+\.([0-9]+)/\1/'
}

__check_version_string() {
  local version="${1}"

  if [ -z "${version}" ]; then
    err "No version string provided."
    return 1
  elif ! echo "${version}" | grep -E -q '^[0-9]+\.[0-9]+\.[0-9]+.*'; then
    err "Version ${GREEN}${version}${NC} does not match expected pattern."
    return 2
  fi
}
# vim:foldenable:foldmethod=indent:foldnestmax=1