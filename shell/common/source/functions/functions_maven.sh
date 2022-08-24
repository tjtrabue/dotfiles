#!/bin/sh

# Bump a Maven project's major version (i.e., 1.0.0 -> 2.0.0).
bumpmavenmajor() {
  local pomXml="${1:-pom.xml}"
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local newVersion="$(bumpmajor "${version}")"

  log_info "Bumping major version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# Bump a Maven project's minor version (i.e., 1.0.0 -> 1.1.0).
bumpmavenminor() {
  local pomXml="${1:-pom.xml}"
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local newVersion="$(bumpminor "${version}")"

  log_info "Bumping minor version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# Bump a Maven project's patch version (i.e., 1.0.0 -> 1.0.1).
bumpmavenpatch() {
  local pomXml="${1:-pom.xml}"
  local version="$(xpath -e '/project/version/text()' "${pomXml}" 2>/dev/null)"
  local newVersion="$(bumppatch "${version}")"

  log_info "Bumping patch version: ${GREEN}${newVersion}${NC}"
  mvn versions:set -DnewVersion="${newVersion}" -DgenerateBackupPoms=false
}

# vim:foldenable:foldmethod=indent:foldnestmax=1