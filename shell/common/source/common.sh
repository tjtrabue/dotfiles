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
COMMON_SOURCE_FILE="${COMMON_SOURCE}/common.sh"
LINUX_SOURCE_DIR="${COMMON_SOURCE}/linux"
# }}}

# Imports {{{
. "${COMMON_SOURCE}/functions/functions_os.sh"
# }}}

# Make sure preparatory steps are taken before running the main profile sourcing
# functions. This should be run before the src() or src_all() functions since it
# takes care of things like setting $PATH correctly on some systems that do not
# automatically have access to required basic resources like the GNU CLI tools.
prepare_shell_for_os() {
  local os="$(getdistro)"

  case "${os}" in
    "Darwin")
      __src_standard_subdirs_under_dir "${COMMON_SOURCE}/mac"
      prepare_mac
      ;;
  esac
}

# Source additional files {{{

# Source files that were transferred from the dotfiles repo, but not linked
# to track future changes.
__src_one_time_transfers() {
  local f

  for f in "${HOME}/.dirs" "${HOME}/.vars"; do
    if [ -f "${f}" ]; then
      . "${f}"
    fi
  done
}

# Source all files in a given directory.
__src_dir() {
  local dir="$1"
  local file

  if [ -z "${dir}" ]; then
    echo "ERROR: No directory provided to __src_dir" 1>&2
    return 1
  fi

  if [ -d "${dir}" ]; then
    for file in $(find "${dir}" -maxdepth 1 -mindepth 1 -type f); do
      . "$file"
    done
  fi
}

# Source additional OS-specific files
__src_os() {
  local archSrcDir="${LINUX_SOURCE_DIR}/arch"
  local macSrcDir="${COMMON_SOURCE}/mac"
  local os

  os="$(getdistro)"

  case "${os}" in
    "Arch Linux")
      __src_standard_subdirs_under_dir "${archSrcDir}"
      ;;

    "Darwin")
      __src_standard_subdirs_under_dir "${macSrcDir}"
      ;;

    *)
      # This warning will quickly become annoying, but is sometimes useful.
      # echo "WARNING: Unknown OS for sourcing: ${os}" 1>&2
      ;;
  esac
}

# Source the aliases, functions, and other directory located under a provided
# directory.
__src_standard_subdirs_under_dir() {
  local baseDir="${1}"
  local d

  for d in "${baseDir}/"{aliases,functions,other}; do
    if [ -d "${d}" ]; then
      __src_dir "${d}"
    fi
  done
}

# Source all functions and alias files for any POSIX-compliant shell.
__src() {
  local currentShell="$(basename "${SHELL}")"
  local srcDir
  local f
  local d

  # Source this file
  . "${COMMON_SOURCE_FILE}"

  # Determine in which directory our shell-specific aliases and functions
  # reside.
  if [ "${currentShell}" = "bash" ]; then
    srcDir="${DOTFILES_BASH}/source"
    # Also load readline bindings if using Bash.
    # NOTE: We only bind the readline file if our shell is interactive.
    [ -f "${HOME}/.inputrc" ] && echo "$-" | grep -q ".*i.*" &&
    bind -f "${HOME}/.inputrc"
  elif [ "${currentShell}" = "zsh" ]; then
    srcDir="${DOTFILES_ZSH}/source"
  fi

  # Source .vars and .dirs.
  __src_one_time_transfers

  # Source all alias/function/other files, both in the common source directory
  # and in current shell's source directory.
  for d in $(find "${COMMON_SOURCE}" "${srcDir}" \
      -maxdepth 1 -mindepth 1 -type d \
      -name '*functions' -o -name '*aliases' -o -name '*other'); do
    __src_dir "${d}"
  done

  # Source OS-specific aliases and functions.
  __src_os
}

# Start SSH, GPG, and other agents, if necessary.
__src_agents() {
  sshagent
  gpgagent
}

# This function takes care of sourcing rvm, jenv, nvm, etc.
# These version/environment managers all manipulate $PATH, so we MUST call them
# AFTER we've constructed our custom $PATH, otherwise their work will be
# overwritten.
__src_extra_environment_profiles() {
  src_dircolors_for_profile
  src_git_for_profile
  src_java_for_profile
  src_node_for_profile
  src_python_for_profile
  src_ruby_for_profile
  src_asdf_for_profile
  src_fasd_for_profile
  src_thef_for_profile
  src_broot_for_profile
  src_prompt_for_profile
}
# }}}

# Source entire shell environment. This function acts as a single point-of-entry
# for pulling extra definitions into a shell, and for invoking
# rvm/nvm/pyenv/jenv. It should work regardless of the shell in use.
src_all() {
  # Immediately source all function/alias files.
  __src

  # Make sure luarocks are available
  src_lua_path
  # Start SSH and GPG agents
  __src_agents
  # Take care of fixing the SSH authentication socket if logged in over SSH.
  fix_ssh_auth_sock
  # This should come last!!!
  __src_extra_environment_profiles
}

# A leaner source function that just sources aliases/functions instead of the
# entire environment. That is, this function does NOT rebuild $PATH, does NOT
# affect nvm/rvm/pyenv/jenv. It only re-sources functions and aliases.
#
# You won't want to continually revert to the default environment throughout
# your shell session, so this function allows you to pull in newer alias and
# function definitions without affecting the wider environment of rvm, nvm,
# jenv, pyenv, etc.
src() {
  __src
}

# vim:foldenable:foldmethod=indent:foldlevel=0:foldnestmax=1
