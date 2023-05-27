#!/bin/sh

# Install Elixir and OTP with asdf. Do not use system package managers, as their
# pre-packaged distributions are often lacking features.

# Install the Elixir language server protocol implementation.
# The install prefix for the libraries and executables may be provided as an
# optional argument.
install_elixir_ls() {
  local installPrefix="${1:-${HOME}}"

  __check_elixir_installed &&
    __download_latest_elixir_ls_dist &&
    __create_elixir_ls_launcher_scripts "${installPrefix}"
}

# Make sure we have the correct executables before proceeding.
__check_elixir_installed() {
  if [ ! -x "$(command -v elixir)" ]; then
    err "No elixir executable found on PATH"
    return 1
  elif [ ! -x "$(command -v mix)" ]; then
    err "No mix executable found on PATH"
    return 2
  fi
}

# Clone the elixir-ls project.
__clone_elixir_ls() {
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsRepo="${workspace}/elixir-ls"
  local elixirLsGitUrl="https://github.com/elixir-lsp/elixir-ls.git"

  if [ ! -d "${elixirLsRepo}" ]; then
    (
      log_info "Cloning ElixirLS repository to: ${elixirLsRepo}"
      cd "${workspace}"
      git clone "${elixirLsGitUrl}"
    )
  else
    warn "ElixirLS git repository already found at: ${elixirLsRepo}"
  fi
}

# Compile the elixir-ls project and generate Mix archives.
__build_elixir_ls() {
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsRepo="${workspace}/elixir-ls"

  (
    log_info "Building ElixirLS"
    cd "${elixirLsRepo}"
    mix local.hex --force
    mix deps.update --all
    mix compile
  )
}

# Rename previously downloaded versions of elixir-ls under ~/.elixir_ls in
# preparation for downloading a new version.
__increment_elixir_ls_version_dirs() {
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls}"
  local f
  local previousNum

  # Exit early if we have no previous elixir-ls versions to increment.
  if ! ls "${elixirLsHome}"/previous_* >>/dev/null 2>&1; then
    return 0
  fi

  log_info "Incrementing all previous elixir-ls versions"
  (
    # Increment previous elixir-ls versions in reverse so as not to
    # inadvertently overwrite others.
    cd "${elixirLsHome}" &&
      while read -r f || [ -n "${f}" ]; do
        previousNum="${f##*_}"
        log_info "Incrementing previous ${MAGENTA}${previousNum}${NC} to" \
          "${CYAN}$((previousNum + 1))${NC}"
        mv "${f}" "previous_$((previousNum + 1))"
      done <<<"$(find "${elixirLsHome}" -type d -regex '.*_[1-9][0-9]*' \
        -exec basename '{}' \; |
        sort -r)"
  )
}

# Download the latest elixir-ls .zip bundle from GitHub.
__download_latest_elixir_ls_dist() {
  local githubApiUrl="https://api.github.com/repos"
  local elixirLsReleasesUrl="${githubApiUrl}/elixir-lsp/elixir-ls/releases/latest"
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls}"
  local elixirLsDownloadDir="${elixirLsHome}/latest"
  local elixirLsPrevious="${elixirLsHome}/previous_1"
  local elixirLsZipFile="${elixirLsHome}/elixir_ls.zip"
  local latestElixirLsDownloadUrl

  __increment_elixir_ls_version_dirs

  if [ -d "${elixirLsDownloadDir}" ]; then
    log_info "Backing up previous version of elixir-ls to:" \
      "${BLUE}${elixirLsPrevious}${NC}"
    mv "${elixirLsDownloadDir}" "${elixirLsPrevious}"
  fi

  mkdir -p "${elixirLsDownloadDir}"

  # We need to get the lastest download URL for the elixir-ls.
  # It's a tricky, roundabout process, requiring us to check the second-to-last
  # result in the list of latest releases. The last result is the default
  # download, which is usually outdated.
  latestElixirLsDownloadUrl="$(curl -sL -H 'Accept: application/json' \
    "${elixirLsReleasesUrl}" |
    jq '.assets | .[length - 2] | .browser_download_url')"

  log_info "Downloading latest elixir-ls distribution"
  eval "curl -sL ${latestElixirLsDownloadUrl} -o ${elixirLsZipFile}"
  unzip -d "${elixirLsDownloadDir}" "${elixirLsZipFile}"

  log_info "Cleaning up"
  rm -f "${elixirLsZipFile}"
}

__create_elixir_ls_launcher_scripts() {
  local installPrefix="${1:-${HOME}}"
  local installBin="${installPrefix}/bin"
  local elixirLsHome="${ELIXIR_LS_HOME:-${HOME}/.elixir_ls}"
  local elixirLsLatest="${elixirLsHome}/latest"

  install -dm0755 "${installBin}"
  # Create an executable script to launch the language server
  command cat <<EOF >"${installBin}/elixir-ls"
#!/bin/sh

exec ${elixirLsLatest}/language_server.sh
EOF
  chmod 755 "${installBin}/elixir-ls"

  # Create an executable script to launch a debugger program for the LS.
  command cat <<EOF >"${installBin}/elixir-ls-debug"
#!/bin/sh

exec ${elixirLsLatest}/debugger.sh
EOF
  chmod 755 "${installBin}/elixir-ls-debug"
}

# Install the elixis-ls executables.
__install_elixir_ls() {
  # The directory containing the lib/ and bin/ directories for elixir-ls.
  local installPrefix="${1:-${HOME}}"
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsName="elixir-ls"
  local elixirLsRepo="${workspace}/${elixirLsName}"
  local installLib="${installPrefix}/lib"
  local installBin="${installPrefix}/bin"

  (
    cd "${elixirLsRepo}"
    install -dm0755 "${installLib}/${elixirLsName}"
    mix elixir_ls.release -o "${installLib}/${elixirLsName}"

    install -dm0755 "${installBin}"
    # Create an executable script to launch the language server
    command cat <<EOF >"${installBin}/elixir-ls"
#!/bin/sh

exec ${installLib}/${elixirLsName}/language_server.sh
EOF
    chmod 755 "${installBin}/elixir-ls"

    # Create an executable script to launch a debugger program for the LS.
    command cat <<EOF >"${installBin}/elixir-ls-debug"
#!/bin/sh

exec ${installLib}/${elixirLsName}/debugger.sh
EOF
    chmod 755 "${installBin}/elixir-ls-debug"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
