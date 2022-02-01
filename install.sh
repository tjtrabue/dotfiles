#!/usr/bin/env bash

set -Eeuo pipefail

### IMPORTANT DEVELOPER NOTE ###
# You should ALWAYS test any changes you make to this script by running:
#  ./install.sh -vv --fake-home "test"
# The error flags set above are very, very picky about bad coding practices, and
# you'd be amazed at what they preclude. Never assume your changes are safe
# before you test!

# Variable Definitions {{{
# The name of the current executable
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"
# Where dotfiles will be installed (useful for testing the installation on a
# fake home directory).
declare TARGET_HOME="${HOME}"

# This repository's root directory.
declare DOTFILES_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Dotfiles directories (will be set after we've established TARGET_HOME)
declare DOTFILES_HOME
declare DOTFILES_SHELL
declare COMMON_SHELL
declare COMMON_SOURCE
declare DOTFILES_LINK
declare DOTFILES_COPY
declare DOTFILES_ZDOTDIR

# Filesystem directories to create

# Main projects directory
declare WS="${HOME}/workspace"
# Practice projects directory
declare PRAC="${HOME}/practice"
# Directory for installed third-party applications
declare APPS="${HOME}/applications"
# Third party archives directory
declare ARCHIVES="${APPS}/archives"

# Program flags

# Force all actions that would otherwise involve answering a prompt
declare FORCE_INSTALL=false

# Logging control variables
declare LOG_LEVEL=1
declare LOG_TO_FILE=""
# }}}

# OS Preparation {{{

# Perform other preparation steps depending on the current operating system.
# This function runs BEFORE the `setup()` function.
prepare_for_os() {
  set_dotfiles_variables
  source_common_defs

  log_info "Checking for additional preparation steps for OS..."
  local osName="$(uname -s)"

  case "${osName}" in
  "Darwin")
    prepare_for_macos
    ;;
  *)
    log_info "No preparation function found for OS type: ${osName}"
    ;;
  esac
}

# macOS specific preparation.
prepare_for_macos() {
  log_info "Preparing for macOS installation"
  source "${COMMON_SOURCE}/mac/functions/functions_mac.sh"
  # Make sure developer tools are installed
  install_mac_developer_tools
  # Make sure homebrew is installed.
  install_homebrew
  # Install all the regular GNU CLI tools.
  install_gnu_cli_tools_for_mac
  # Make sure the CLI tools we reference throughout this install script are the
  # GNU versions, not the BSD versions which come standard on macOS.
  create_gnu_cli_tool_aliases_for_mac
}
# }}}

# Setup/Cleanup {{{

# Set all dotfiles-related variables after all arguments have been parsed and
# key variables have been set.
set_dotfiles_variables() {
  DOTFILES_HOME="${TARGET_HOME}/.dotfiles"
  DOTFILES_SHELL="${DOTFILES_REPO}/shell"
  COMMON_SHELL="${DOTFILES_SHELL}/common"
  COMMON_SOURCE="${COMMON_SHELL}/source"
  DOTFILES_LINK="${DOTFILES_REPO}/link"
  DOTFILES_COPY="${DOTFILES_REPO}/copy"
  DOTFILES_ZDOTDIR="${DOTFILES_SHELL}/zsh/zdotdir"
}

# Take care of backing up existing ~/.dotfiles directory
backup_existing_installation() {
  local oldDotfilesDir

  # Safe name for backup directory
  oldDotfilesDir="$(mktemp -u "${DOTFILES_HOME}.bak.XXXXXXXXXX")"
  if [ -d "${DOTFILES_HOME}" ]; then
    log_info "Backing up existing dotfiles installation to ${oldDotfilesDir}"
    mv "${DOTFILES_HOME}" "${oldDotfilesDir}"
  fi
}

# Check for an existing dotfiles installation at $DOTFILES_HOME.
check_existing_installation() {
  log_info "Checking for existing dotfiles installation"
  test -h "${DOTFILES_HOME}" || test -d "${DOTFILES_HOME}"
}

# Figure out what to do if an existing dotfiles installation is found.
remove_existing_installation() {
  local response=""

  command cat <<EOF
An existing dotfiles installation was found at ${DOTFILES_HOME}.
It must be removed before this installation can progress.
EOF

  while ! echo "${response}" | grep -q '^[YyNn]$'; do
    echoe "Remove it and continue with the installation? [y/n]"
    IFS="" read -r response
  done

  if echo "${response}" | grep -q '^[Nn]$'; then
    echoe "Exiting dotfiles installation."
    exit 1
  elif [ -h "${DOTFILES_HOME}" ]; then
    log_info "Removing old dotfiles symlink"
    rm -f "${DOTFILES_HOME}"
  else
    log_info "Removing old dotfiles installation"
    rm -rf "${DOTFILES_HOME}"
  fi
}

# Performs initial setup.
setup() {
  log_info "Setting up..."
  if ! ${FORCE_INSTALL}; then
    check_existing_installation && remove_existing_installation
  fi
  backup_existing_installation
  ensure_dirs_present
}
# }}}

# Help {{{

_help() {
  command cat <<EOF
${THIS_EXEC}

Install tjtrabue's dotfiles on the current system.  For the most part, this
script just creates a bunch of symlinks, so it is highly non-destructive.  As
opposed to overwriting the user's existing dotfiles, this script backs up all
of the existing files before creating any symlinks. Nothing should be lost in
the process. Check the 'backup' directory created by this script if you wish to
restore your old dotfiles.

USAGE:
  ${THIS_EXEC} [OPTIONS]

OPTIONS:
  -h | --help
    Print the help message (this message) and exit.

  -v | --verbose
    Increase the logging output level. This command may be specified multiple
    times to further increase verbosity of output.

  -f | --force
    Force dotfiles to install, assuming "yes" for all prompts.
    This option should be used with caution, as it may overwrite some of your
    files, even though this script tries hard not to do that.

  -k | --fake-home <target_directory>
    Install dotfiles to a different directory. The default is to install
    dotfiles to the user's \$HOME directory. This option changes the default
    behavior, telling the install script to instead install everything to a
    different directory. This only real use for this option is to test the
    install script.

EXAMPLES:
  Install dotfiles with INFO logging output:
    ./install.sh -vv

  Force a new dotfiles installation, wihtout prompting for confirmation:
    ./install.sh --force

  Test the install script by installing dotfiles to a fake home directory:
    ./install.sh -vv --fake-home ./test
EOF
}

# }}}

# Primary Functions {{{

# Link files/directories to the ~/.config directory.
link_config() {
  local dotfilesConfig="${DOTFILES_LINK}/config"
  local homeConfig="${TARGET_HOME}/.config"
  local homeConfigBackup="${homeConfig}.bak"

  log_info "Linking ${dotfilesConfig} to ${homeConfig}"

  if [ -d "${homeConfig}" ]; then
    log_info "Backing up files in ${homeConfig} to ${homeConfigBackup}"
    mv -f "${homeConfig}" "${homeConfigBackup}"
  fi

  ln -sf "${dotfilesConfig}" "${homeConfig}"

  if [ -d "${homeConfigBackup}" ]; then
    log_info "Restoring files from ${homeConfigBackup} to ${homeConfig}"
    rsync -ah "${homeConfigBackup}/" "${homeConfig}"
    rm -rf "${homeConfigBackup}"
  fi

  log_info "Done."
}

# Link the LSP configuration dir to ~/.lsp
link_lsp_config() {
  local lspConfigDir="${DOTFILES_LINK}/lsp"
  local lspConfigTarget="${HOME}/.lsp"

  if [ -d "${lspConfigTarget}" ]; then
    rm -rf "${lspConfigTarget}"
  fi
  ln -sf "${lspConfigDir}" "${lspConfigTarget}"
}

# Link the repository itself, if necessary.
link_repo() {
  log_info "Linking dotfiles repository to: ${DOTFILES_HOME}"
  if [ "${DOTFILES_REPO}" != "${DOTFILES_HOME}" ]; then
    ln -sf "${DOTFILES_REPO}" "${DOTFILES_HOME}"
  fi
  log_info "Done."
}

# Create dotfile symlinks from user's home dir to those managed by the dotfiles
# repository. This keeps the user's dotfiles in sync with the repository.
# Use the `dotsync` function to keep dotfiles up to date after the initial
# installation.
link_dotfiles() {
  log_info "Linking dotfiles"
  find "${DOTFILES_LINK}/home" -type f -exec ln -sfb -t "${TARGET_HOME}" '{}' \;
  log_info "Done"
}

# Link the Zsh dotfiles directory to ~/.zsh
link_zdotdir() {
  local targetZdotdir="${TARGET_HOME}/.zsh"

  log_info "Linking Zsh directory ${DOTFILES_ZDOTDIR} to ${targetZdotdir}"
  if [ -h "${targetZdotdir}" ]; then
    rm -f "${targetZdotdir}"
  elif [ -d "${targetZdotdir}" ]; then
    rm -rf "${targetZdotdir}"
  fi

  ln -sf "${DOTFILES_ZDOTDIR}" "${targetZdotdir}"
}

# Copy one-time transfer files.
copy_dotfiles() {
  local oneTimeTransfersDir="${DOTFILES_COPY}/dotfiles_to_copy"

  log_info "Copying dotfiles from: ${BLUE}${oneTimeTransfersDir}${NC}"

  find "${oneTimeTransfersDir}" -maxdepth 1 -mindepth 1 -type f \
    -exec cp -f -t "${TARGET_HOME}/" '{}' \;
  log_info "Copying complete"
}

# Create important directories.
ensure_dirs_present() {
  log_info "Creating important directories"

  local dirs=(
    "${TARGET_HOME}"
    "${WS}"
    "${PRAC}"
    "${APPS}"
    "${ARCHIVES}"
  )
  local dir

  for dir in "${dirs[@]}"; do
    mkdir -p "${dir}" &>/dev/null
  done
}

# Import common aliases and functions for use within this script.
source_common_defs() {
  local f
  for f in "${COMMON_SOURCE}"/{aliases,functions,other}/*; do
    . "${f}"
  done
}

# Main that calls all subroutines
main() {
  setup
  copy_dotfiles
  link_repo
  link_dotfiles
  link_zdotdir
  link_config
  add_extra_os_vars
  add_extra_paths_to_path_file
}
# }}}

# Need to prepare OS before CLI option parsing because we may not even have
# access to GNU getopt yet.
prepare_for_os

# Parse CLI Options {{{
args=$(getopt -o hvfk: \
  --long help,verbose,force,fake-home: \
  -n 'install.sh' \
  -- "$@")
eval set -- "$args"

# extract options and their arguments into variables.
while true; do
  case "$1" in
  -h | --help)
    _help
    shift
    exit 0
    ;;

  -v | --verbose)
    ((LOG_LEVEL += 1))
    shift
    ;;

  -f | --force)
    FORCE_INSTALL=true
    shift
    ;;

  -k | --fake-home)
    case "$2" in
    "")
      shift 2
      ;;
    *)
      TARGET_HOME="${2}"
      shift 2
      ;;
    esac
    ;;

  --)
    shift
    break
    ;;

  *)
    err "Unknown option $1 to ${THIS_EXEC}"
    exit 2
    ;;
  esac
done
# }}}

# Main execution
main

# Modeline for this file (KEEP IT COMMENTED!)
# vim:foldenable:foldmethod=marker:foldlevel=0
