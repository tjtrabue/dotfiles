#!/usr/bin/env bash

# Variable Definitions {{{
# Directories
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"
declare DOTFILES_HOME="$HOME/.dotfiles"
declare DOTFILES_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare DOTFILES_SHELL="${DOTFILES_REPO}/shell"
declare COMMON_SHELL="${DOTFILES_SHELL}/common"
declare COMMON_SOURCE="${COMMON_SHELL}/source"
declare DOTFILES_LINK="${DOTFILES_REPO}/link"
declare DOTFILES_COPY="${DOTFILES_REPO}/copy"

# Filesystem directories to create

# Main projects directory
declare WS="${HOME}/workspace"
# Practice projects directory
declare PRAC="${WS}/practice"
# Directory for installed third-party applications
declare APPS="${HOME}/applications"
# Third party archives directory
declare ARCHIVES="${APPS}/archives"

# Program flags

# Force all actions that would otherwise involve answering a prompt
declare FORCE_INSTALL=false
declare LOG_LEVEL=1
declare LOG_TO_FILE=""
# }}}

# Setup/Cleanup {{{

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

# Figure out what to do if an existing dotfiles installation is found.
check_existing_installation() {
  log_info "Checking for existing dotfiles installation"
  local response=""

  if [ -d "${DOTFILES_HOME}" ]; then
    warn "Existing dotfiles installation found at ${DOTFILES_HOME}!"
    cat <<EOF
An existing dotfiles installation was found at ${DOTFILES_HOME}.
It must be removed before this installation can progress.
EOF
    while ! echo "${response}" | grep -q "[YyNn]"; do
      echoe "Would you like to remove it and continue with the installation?" \
        "[y/n]"
      read -sn1 response
    done
    echo "${response}" | grep -q "[Nn]" && echoe "Exiting." && exit 1
  else
    log_info "No existing dotfiles installation found."
  fi
}

# Source all common aliases and functions.
src() {
  local f
  for f in "${COMMON_SOURCE}"/{aliases,functions}/*; do
    . "${f}"
  done
}
# We need to run this immediately. Can't wait for main to load.
src

# Performs initial setup.
setup() {
  log_info "Setting up..."
  if ! ${FORCE_INSTALL}; then
    check_existing_installation
  fi
  backup_existing_installation
}
# }}}

# Help {{{

_help() {
  cat <<EOF
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

  -f | --force
    Force dotfiles to install, assuming "yes" for all prompts.
    This option should be used with caution, as it may overwrite some of your
    files, even though this script tries hard not to do that.
EOF
}

# }}}

# Test Functions {{{
print_vars() {
  echoe "DOTFILES_REPO: ${DOTFILES_REPO}"
  echoe "DOTFILES_HOME: ${DOTFILES_HOME}"
}
# }}}

# Primary Functions {{{

# Link files/directories to the ~/.config directory.
link_config() {
  local dotfilesConfig="${DOTFILES_LINK}/config"
  local homeConfig="${HOME}/.config"
  local homeConfigBackup="${homeConfig}.bak"

  log_info "Linking ${dotfilesConfig} to \~/.config"

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

  succ "Done."
}

# Create dotfile symlinks from user's home dir to those managed by the dotfiles
# repository. This keeps the user's dotfiles in sync with the repository.
# Use the `dotsync` function to keep dotfiles up to date after the initial
# installation.
link_dotfiles() {
  log_info "Linking dotfiles"
  ln -sfn "${DOTFILES_REPO}" "${DOTFILES_HOME}"
  find "${DOTFILES_LINK}/home" -type f -exec ln -sfb -t "${HOME}" '{}' \;
  succ "Linking complete."
}

# Copy one-time transfer files.
copy_dotfiles() {
  log_info "Copying dotfiles"
  find "${DOTFILES_COPY}" -maxdepth 1 -mindepth 1 -type f \
    -exec cp -f '{}' "${HOME}/" \;
  succ "Copying complete"
}

add_extra_os_vars() {
  local os="$(getosinfo | head -1 | sed 's/Distribution:\s*//')"
  local extraVarsDir="${DOTFILES_REPO}/copy/var_files"
  local extraVarsLinuxDir="${extraVarsDir}/linux"
  local markerString="#<additional-vars-insert>"
  local extraVarsFile

  log_info "Injecting additional OS variables into ${HOME}/.vars"

  case "${os}" in
  "Arch Linux")
    extraVarsFile="${extraVarsLinuxDir}/arch_vars.bash"
    ;;

  *)
    log_info "No extra vars to add for OS: ${os}"
    ;;
  esac

  if [ -f "${extraVarsFile}" ]; then
    sed -i -e "/${markerString}/r ${extraVarsFile}" "${HOME}/.vars"
  fi

  # Get rid of marker string in ~/.vars
  sed -i "/${markerString}/d" "${HOME}/.vars"
  succ "Done injecting additional variables"
}

ensure_dirs_present() {
  log_info "Creating important directories"

  local dirs=(
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

# Main that calls all subroutines
main() {
  setup
  copy_dotfiles
  link_dotfiles
  ensure_dirs_present
  link_config
  add_extra_os_vars
}
# }}}

# Parse CLI Options {{{
args=$(getopt -o hvf --long help,verbose,force -n 'init_arch' -- "$@")
eval set -- "$args"

# extract options and their arguments into variables.
while true; do
  case "$1" in
  -v | --verbose)
    VERBOSE=true
    shift
    ;;

  -h | --help)
    _help
    shift
    exit 0
    ;;

  -v | --verbose)
    shift
    ;;

  -f | --force)
    FORCE_INSTALL=true
    shift
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
# vim:foldenable:foldmethod=marker
