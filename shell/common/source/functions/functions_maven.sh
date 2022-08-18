#!/bin/sh

# Bump a Maven project's major version (i.e., 1.0.0 -> 2.0.0).
bumpmajor() {
  local pomXml="pom.xml"
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local suffix="$(echo "${version}" | sed -E 's/.*(-.*)/\1/')"
  local nonSnapshotVersion="$(echo "${version}" | sed 's/-.*$//')"
  local majorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/([0-9]+)\.[0-9]+\.[0-9]+/\1/')"
  local minorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.([0-9]+)\.[0-9]+/\1/')"
  local buxfixVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.[0-9]+\.([0-9]+)/\1/')"
  local newVersion="$((majorVersion += 1)).${minorVersion}.${buxfixVersion}${suffix}"

  log_info "New version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# Bump a Maven project's minor version (i.e., 1.0.0 -> 1.1.0).
bumpminor() {
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local suffix="$(echo "${version}" | sed -E 's/.*(-.*)/\1/')"
  local nonSnapshotVersion="$(echo "${version}" | sed 's/-.*$//')"
  local majorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/([0-9]+)\.[0-9]+\.[0-9]+/\1/')"
  local minorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.([0-9]+)\.[0-9]+/\1/')"
  local buxfixVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.[0-9]+\.([0-9]+)/\1/')"
  local newVersion="${majorVersion}.$((minorVersion += 1)).${buxfixVersion}${suffix}"

  log_info "New version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# Bump a Maven project's bugfix version (i.e., 1.0.0 -> 1.0.1).
bumpbugfix() {
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local suffix="$(echo "${version}" | sed -E 's/.*(-.*)/\1/')"
  local nonSnapshotVersion="$(echo "${version}" | sed 's/-.*$//')"
  local majorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/([0-9]+)\.[0-9]+\.[0-9]+/\1/')"
  local minorVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.([0-9]+)\.[0-9]+/\1/')"
  local buxfixVersion="$(echo "${nonSnapshotVersion}" |
    sed -E 's/[0-9]+\.[0-9]+\.([0-9]+)/\1/')"
  local newVersion="${majorVersion}.${minorVersion}.$((buxfixVersion += 1))${suffix}"

  log_info "New version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# vim:foldenable:foldmethod=indent:foldnestmax=1