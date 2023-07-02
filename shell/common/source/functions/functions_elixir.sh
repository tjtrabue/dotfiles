#!/bin/sh

# Install Elixir and OTP with asdf. Do not use system package managers, as their
# pre-packaged distributions are often lacking features.

# Install the Elixir language server protocol implementation.
# The install prefix for the libraries and executables may be provided as an
# optional argument.
install_elixir_ls() {
  local elixirLsVersionToDownload="${1:-latest}"
  local installPrefix="${2:-${HOME}}"
  local response=""

  if [ "${elixirLsVersionToDownload}" = "latest" ]; then
    elixirLsVersionToDownload="$(__get_latest_elixir_ls_version)"
  fi

  __download_elixir_ls_dist "${elixirLsVersionToDownload}" &&
    while ! echo "${response}" | grep -q "[YyNn]"; do
      command cat <<EOF
Would you like to set the current version of elixir-ls to:
${GREEN}${elixirLsVersionToDownload}${NC}? [y/n]
EOF
      read -r response
    done &&
    if echo "${response}" | grep -q "[Yy]"; then
      set_current_elixir_ls "${elixirLsVersionToDownload}" "${installPrefix}"
    fi
}

__download_elixir_ls_metadata() {
  local elixirLsReleaseJson="${1:-/tmp/elixir-ls-release.json}"
  local githubApiUrl="https://api.github.com/repos"
  local elixirLsReleasesUrl="${githubApiUrl}/elixir-lsp/elixir-ls/releases/latest"

  log_info "Download elixir-ls release info file to:" \
    "${BLUE}${elixirLsReleaseJson}${NC}"
  curl -sL -H 'Accept: application/json' "${elixirLsReleasesUrl}" \
    -o "${elixirLsReleaseJson}"
}

# Create a symlink representing the user's currently selected version of
# elixir-ls.
set_current_elixir_ls() {
  local currentVersion="${1#elixir-ls-}"
  local installPrefix="${2:-${HOME}}"
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls_installs}"
  local selectedElixirLsRelease="${elixirLsHome}/elixir-ls-${currentVersion}"
  local currentElixirLsSymlink="${elixirLsHome}/current"

  if [ ! -d "${selectedElixirLsRelease}" ]; then
    err "No such elixir-ls installation: ${BLUE}${selectedElixirLsRelease}${NC}"
    return 1
  fi

  log_info "Setting current elixir-ls to: ${BLUE}${selectedElixirLsRelease}${NC}"
  rm -f "${currentElixirLsSymlink}"
  ln -sf "${selectedElixirLsRelease}" "${currentElixirLsSymlink}"
  __link_elixir_ls_scripts "${currentElixirLsSymlink}" "${installPrefix}"
}

__get_latest_elixir_ls_version() {
  local elixirLsReleaseJson="${1:-/tmp/elixir-ls-release.json}"
  local elixirLsReleaseDownloadUrl
  local latestElixirLsVersion

  if [ ! -f "${elixirLsReleaseJson}" ]; then
    __download_elixir_ls_metadata "${elixirLsReleaseJson}"
  fi

  elixirLsReleaseDownloadUrl="$(jq '.' "${elixirLsReleaseJson}" |
    grep 'browser_download_url' |
    sed 's/^\s*//' |
    tail -2 |
    head -1 |
    awk '{print $2}' |
    sed -e 's/^"//' -e 's/"$//')"

  latestElixirLsVersion="$(echo "${elixirLsReleaseDownloadUrl}" |
    sed -E 's/^.*elixir-ls-(.*).zip/\1/')"

  log_info "Latest elixir-ls version: ${MAGENTA}${latestElixirLsVersion}${NC}"
  printf '%s' "${latestElixirLsVersion}"
}

# Download a specific elixir-ls release, such as '1.14-25.1'.
#
# The downloaded ZIP file resides in:
#   ~/.elixir_ls_installs/downloads/
# The actual elixir-ls installation resides in:
#   ~/.elixir_ls_installs/elixir-ls-VERSION/
__download_elixir_ls_dist() {
  local elixirLsVersionToDownload="${1:-latest}"
  local elixirLsReleaseJson="/tmp/elixir-ls-release.json"
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls_installs}"
  local elixirLsDownloadsDir="${elixirLsHome}/downloads"
  local elixirLsReleaseDownloadUrl
  local elixirLsReleaseBasename
  local elixirLsReleaseZip
  local elixirLsReleaseDir

  if [ ! -f "${elixirLsReleaseJson}" ]; then
    __download_elixir_ls_metadata "${elixirLsReleaseJson}"
  fi

  if [ "${elixirLsVersionToDownload}" = 'latest' ]; then
    elixirLsVersionToDownload="$(__get_latest_elixir_ls_version \
      "${elixirLsReleaseJson}")"
  fi

  log_info "Downloading elixir-ls release:" \
    "${MAGENTA}${elixirLsVersionToDownload}${NC}"
  elixirLsReleaseDownloadUrl="$(jq '.' "${elixirLsReleaseJson}" |
    grep "browser_download_url.*elixir-ls-${elixirLsVersionToDownload}\.zip" |
    sed 's/^\s*//' |
    awk '{print $2}' |
    sed -e 's/^"//' -e 's/"$//')"

  elixirLsReleaseBasename="elixir-ls-$(echo "${elixirLsReleaseDownloadUrl}" |
    sed -E 's/^.*elixir-ls-(.*).zip/\1/')"
  elixirLsReleaseDir="${elixirLsHome}/${elixirLsReleaseBasename}"
  elixirLsReleaseZip="${elixirLsDownloadsDir}/${elixirLsReleaseBasename}.zip"

  curl -sL "${elixirLsReleaseDownloadUrl}" \
    --create-dirs \
    -o "${elixirLsDownloadsDir}/elixir-ls-${elixirLsVersionToDownload}.zip"

  mkdir -p "${elixirLsReleaseDir}"
  unzip -d "${elixirLsReleaseDir}" "${elixirLsReleaseZip}"

  rm -f "${elixirLsReleaseJson}"
}

# Symlink the elixir-ls language server and debugger scripts to an executable
# file directory.
__link_elixir_ls_scripts() {
  local elixirLsReleaseDir="${1}"
  local installPrefix="${2:-${HOME}}"
  local installBin="${installPrefix}/bin"
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls_installs}"

  log_info "Linking elixir-ls scripts in ${BLUE}${elixirLsReleaseDir}${NC} to" \
    "${BLUE}${installBin}${NC}"
  ln -sf "${elixirLsReleaseDir}/language_server.sh" "${installBin}/elixir-ls"
  ln -sf "${elixirLsReleaseDir}/debugger.sh" "${installBin}/elixir-ls-debug"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
