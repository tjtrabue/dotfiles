#!/bin/sh

# This file performs common initialization tasks for POSIX shells.
# This file should be sourced as soon as possible by any POSIX-compliant
# initialization file for the shell (i.e, by .bashrc or .zshrc) so that
# common functions and aliases can be brought into the shell's environment.

# Variables {{{
DOTFILES_HOME="${HOME}/.dotfiles"
DOTFILES_SHELL="${DOTFILES_HOME}/shell"
COMMON_SHELL="${DOTFILES_SHELL}/common"
DOTFILES_BASH="${DOTFILES_SHELL}/bash"
DOTFILES_ZSH="${DOTFILES_SHELL}/zsh"
COMMON_SOURCE="${COMMON_SHELL}/source"
LINUX_SOURCE_DIR="${COMMON_SOURCE}/linux"
# }}}

# Source additional files {{{

# Source files that were transferred from the dotfiles repo, but not linked
# to track future changes.
__src_one_time_transfers() {
  local file

  for file in "${HOME}"/.{dirs,vars}; do
    [ -f "${file}" ] && . "${file}"
  done
}

# Source all files in a given directory.
__src_in_dir() {
  local dir="$1"
  local file

  if [ -z "$dir" ]; then
    echo "ERROR: No directory provided to function ${FUNCNAME[0]}" 1>&2
    return 1
  fi

  if [ -d "$dir" ]; then
    for file in $(find "$dir" -maxdepth 1 -mindepth 1 -type f); do
      . "$file"
    done
  fi
}

# Source additional OS-specific files
__src_os() {
  local archSrcDir="${LINUX_SOURCE_DIR}/arch"
  local os

  # Make sure to get reference to "getosinfo" function
  . "${COMMON_SOURCE}/functions/functions_os.sh"
  os="$(getosinfo | head -1 | sed 's/Distribution:\s*//')"

  case "${os}" in
  "Arch Linux")
    __src_in_dir "${archSrcDir}/aliases"
    __src_in_dir "${archSrcDir}/functions"
    ;;

  *)
    echo "WARNING: Unknown OS for sourcing: ${os}" 1>&2
    ;;
  esac
}

# Source all functions and alias files for any POSIX-compliant shell.
src() {
  local currentShell="$(ps -p $$ | awk '{print $NF}' | tail -1)"
  local srcDir
  local f
  local d

  # Determine in which directory our shell-specific aliases and functions
  # reside.
  if [ "${currentShell}" = "bash" ]; then
    srcDir="${DOTFILES_BASH}/source"
    # Also load readline bindings if using Bash.
    [ -f "${HOME}/.inputrc" ] && bind -f "${HOME}/.inputrc"
  elif [ "${currentShell}" = "zsh" ]; then
    srcDir="${DOTFILES_ZSH}/source"
  fi

  # Source .vars and .dirs.
  __src_one_time_transfers

  # Source all files in all directories under *source/
  for d in $(find "$COMMON_SOURCE" -maxdepth 1 -mindepth 1 -type d); do
    __src_in_dir "${d}"
  done
  for d in $(find "${srcDir}" -maxdepth 1 -mindepth 1 -type d); do
    __src_in_dir "${d}"
  done

  # Source OS-specific aliases and functions.
  __src_os
}
# }}}

# Main entry point for this module.
main() {
  # Immediately source all function/alias files.
  src
  # Add extra binary paths to $PATH
  spath
  # Make sure luarocks are available
  src_lua_path
}
main

# vim:foldenable:foldmethod=marker:foldlevel=0
