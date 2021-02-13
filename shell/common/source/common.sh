#!/bin/sh

# This file performs common initialization tasks for POSIX shells.
# This file should be sourced as soon as possible by any POSIX-compliant
# initialization file for the shell (i.e, by .bashrc or .zshrc) so that
# common functions and aliases can be brought into the shell's environment.

# Variables {{{
DOTFILES_HOME="${HOME}/.dotfiles"
COMMON_SHELL="${DOTFILES_HOME}/shell/common"
COMMON_SOURCE="${COMMON_SHELL}/source"
DOTFILES_BASH="${DOTFILES_HOME}/shell/bash"
DOTFILES_ZSH="${DOTFILES_HOME}/shell/zsh"
LINUX_SOURCE_DIR="${COMMON_SOURCE}/linux"
# }}}

# Source additional files {{{

# Source files that were transferred from the dotfiles repo, but not linked
# to track future changes.
__src_one_time_transfers() {
  local file

  for file in "${HOME}"/.{dirs,vars}; do
    . "${file}"
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
    echo "ERROR: Unknown OS for sourcing: ${os}" 1>&2
    return 1
    ;;
  esac
}

# Pull in ANSI colors as variables.
__src_colors() {
  local colorsFile="${COMMON_SOURCE}/colors.sh"
  [ -f "${colorsFile}" ] && . "${colorsFile}"
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

  # Pull in ANSI color variables
  __src_colors

  # Main alias/function sourcing logic
  for d in {aliases,functions}; do
    __src_in_dir "${COMMON_SOURCE}/${d}"
    __src_in_dir "${srcDir}/${d}"
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
