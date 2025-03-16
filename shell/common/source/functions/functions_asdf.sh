#!/bin/sh

# Functions for interacting with the asdf version manager.

# Clone the asdf Git repository to ~/.asdf
clone_asdf() {
  local asdfDir="${ASDF_DIR:-${HOME}/.asdf}"
  local asdfGitRepoUrl="https://github.com/asdf-vm/asdf.git"

  if [ ! -d "${asdfDir}" ]; then
    log_info "Cloning asdf Git repository"
    git clone "${asdfGitRepoUrl}" "${asdfDir}"
  else
    warn "asdf directory ${asdfDir} already exists!"
  fi
}

# Add all asdf language plugins configured for this repository.
add_asdf_plugins() {
  local plugin

  if [ ! -f "${ASDF_PLUGINS_FILE}" ]; then
    err "No asdf plugins file defined."
    return 1
  fi

  if [ ! -x "$(command -v asdf)" ]; then
    err "asdf executable not found on PATH."
    return 2
  fi

  log_info "Adding asdf plugins..."
  while IFS="" read -r plugin || [ -n "${plugin}" ]; do
    log_info "Adding asdf plugin: ${plugin}"
    asdf plugin add "${plugin}"
  done <"${ASDF_PLUGINS_FILE}"
}

# Download and install a given ASDF version from the archive page on GitHub.
install_asdf() {
  local gitHubDownloadUrlBase="https://github.com/asdf-vm/asdf/releases/download"
  local asdfVersion="${1:-v0.16.5}"
  # One of: 386, amd64, arm64
  local processorArch="${2:-386}"
  local archiveFile="asdf-${asdfVersion}-linux-${processorArch}.tar.gz"
  local downloadDir="/tmp"
  local destinationDir="${HOME}/.local/bin"

  mkdir -p "${destinationDir}"
  log_info "Downloading ASDF version ${CYAN}${asdfVersion}${NC}"
  (
    cd "${downloadDir}" &&
      wget "${gitHubDownloadUrlBase}/${asdfVersion}/${archiveFile}" &&
      tar -zxvf "${archiveFile}" &&
      mv "asdf" "${destinationDir}/"
  )
}

# Wrapper function for updating all installed asdf plugins.
update_asdf() {
  if [ -n "$(command -v asdf)" ]; then
    log_info "Updating ASDF plugins"
    asdf plugin update --all
  else
    warn 'asdf executable not found on $PATH'
  fi
}

# Build and install the latest Erlang/OTP using ASDF.
asdf_install_erlang() {
  export KERL_BUILD_DOCS="yes"
  asdf install erlang latest
  asdf global erland "$(asdf latest erlang)"
}

# Install the latest Elixir version with ASDF and symlink it to
# /home/build/elixir so elixir-ls can find the source files.
asdf_install_elixir() {
  local globalBuildDir="/home/build"
  local globalElixirBuildDir="${globalBuildDir}/elixir"
  local asdfDir="${HOME}/.asdf"
  local latestElixirVersion
  local asdfElixirInstallDir

  log_info "Installing latest Elixir version with ASDF"
  asdf install elixir latest
  latestElixirVersion="$(asdf latest elixir)"
  asdfElixirInstallDir="${asdfDir}/installs/elixir/${latestElixirVersion}"
  asdf global elixir "${latestElixirVersion}"

  log_info "Symlinking ${BLUE}${asdfElixirInstallDir}{NC} to" \
    "${BLUE}${globalElixirBuildDir}${NC} for elixir-ls"
  sudo mkdir -p "${globalBuildDir}"
  sudo ln -s "${asdfElixirInstallDir}" "${globalElixirBuildDir}"
}

# Takes care of the nitty gritty sourcing logic for asdf.
__src_asdf() {
  local currentShell="$(currentshell)"
  local asdfDir="${ASDF_DIR:-${HOME}/.asdf}"
  local asdfSourceFile="${asdfDir}/asdf.sh"
  local asdfSourceFileFish="${asdfDir}/asdf.fish"

  log_info "Sourcing asdf for shell: ${currentShell}"

  case "${currentShell}" in
  "bash")
    . "${asdfSourceFile}"
    ;;
  "zsh")
    . "${asdfSourceFile}"
    ;;
  "fish")
    source "${asdfSourceFileFish}"
    ;;
  *)
    err "Shell ${currentShell} not supported by asdf."
    return 1
    ;;
  esac
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
