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

# Source additional files {{{

# Source files important for initializing basic environment variables and
# properties needed by functions and aliases. This should be the first sourcing
# function called in most circumstances.
__src_env_setup_files() {
  local f

  for f in "${HOME}/."{vars,var_overrides,dirs}; do
    [ -s "${f}" ] && . "${f}"
  done
}

# Source shell files local to the machine (i.e., not under version control).
# These files are designed to hold extra configuration important for the user's
# daily tasks on their machine but is not portable. Thus, any work-specific or
# computer-specific configuration should go in these files.
#
# NOTE: The config in these files should take precedence over anything
# committed to this repository. This allows for machine-local overriding of
# default configuration.
__src_machine_local_files() {
  local machineLocalFilesDir="${HOME}/.extra"
  local f

  # This test is necessary for determining whether the ~/.extra directory is
  # empty. If it is empty, we want to ignore this sourcing step altogether
  # because that would break our workflwo.
  if [ -d "${machineLocalFilesDir}" ] &&
    [ -n "$(command ls -A "${machineLocalFilesDir}")" ]; then
    for f in "${machineLocalFilesDir}"/*; do
      [ -s "${f}" ] && . "${f}"
    done
  elif [ -s "${machineLocalFilesDir}" ]; then
    # The ~/.extra directory used to be a file, so we should include this case
    # for backwards compatibility.
    . "${machineLocalFilesDir}"
  fi
}

# Source all files in a given directory.
__src_dir() {
  local dir="${1}"
  local f

  if [ -z "${dir}" ]; then
    echo "ERROR: No directory provided to __src_dir" 1>&2
    return 1
  fi

  # Check that dir exists and is not empty.
  if [ -d "${dir}" ] && [ -n "$(command ls -A "${dir}")" ]; then
    for f in "${dir}"/*; do
      [ -s "${f}" ] && . "${f}"
    done
  fi
}

# Source OS-specific function/alias files.
__src_os() {
  local archSrcDir="${LINUX_SOURCE_DIR}/arch"
  local ubuntuSrcDir="${LINUX_SOURCE_DIR}/ubuntu"
  local macSrcDir="${COMMON_SOURCE}/mac"
  local wslSrcDir="${COMMON_SOURCE}/wsl"
  local osType
  local os

  osType="$(getostype)"
  os="$(getdistro)"

  case "${osType}" in
  "Linux")
    __src_standard_subdirs_under_dir "${LINUX_SOURCE_DIR}"
    if osiswsl; then
      # Also source WSL definitions if running as a WSL system in Windows.
      __src_standard_subdirs_under_dir "${wslSrcDir}"
    fi
    ;;
  "Darwin")
    __src_standard_subdirs_under_dir "${macSrcDir}"
    ;;
  *)
    # This warning will quickly become annoying, but is sometimes useful.
    # echo "WARNING: Unknown OS for sourcing: ${os}" 1>&2
    ;;
  esac

  # Source distribution-specific Linux function/alias files.
  case "${os}" in
  "Arch Linux")
    __src_standard_subdirs_under_dir "${archSrcDir}"
    ;;
  "Ubuntu")
    __src_standard_subdirs_under_dir "${ubuntuSrcDir}"
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

# Bash and some other shells rely on the readline library for command line
# editing capabilities. Thus, we may need to re-activate bindings in the
# readline init file, named ~/.inputrc, for those shells.
__src_readline_init_file() {
  local readlineInitFile="${HOME}/.inputrc"

  # NOTE: We only bind the readline file if our shell is interactive.
  if [ -s "${readlineInitFile}" ] && echo "$-" | grep -q ".*i.*"; then
    bind -f "${readlineInitFile}"
  fi
}

# Source all functions and alias files for any POSIX-compliant shell.
__src() {
  local srcDir=""

  # Source .vars, .var_overrides, and .dirs.
  __src_env_setup_files

  # Source all alias/function/other files common to all shells.
  __src_standard_subdirs_under_dir "${COMMON_SOURCE}"

  # Figure out which UNIX shell we are using.
  local currentShell="$(currentshell)"

  # Determine in which directory our shell-specific aliases and functions
  # reside.
  if [ "${currentShell}" = "bash" ]; then
    srcDir="${DOTFILES_BASH}/source"
    # Also load readline bindings if using Bash.
    __src_readline_init_file
  elif [ "${currentShell}" = "zsh" ]; then
    srcDir="${DOTFILES_ZSH}/source"
  fi

  # Source all alias/function/other files specific to the user's current shell.
  __src_standard_subdirs_under_dir "${srcDir}"

  # Source OS-specific aliases and functions.
  __src_os

  # Source files in ~/.extra/
  __src_machine_local_files

  # export the dynamically constructed $PATH variable from the entries in
  # ~/.path, as well as CPATH and LIBRARY_PATH if available.
  spath_all

  # Source the dynamically generated ~/.luapath file.
  src_lua_path
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
  src_user_limits_for_profile
  src_dircolors_for_profile
  src_dart_for_profile
  src_git_for_profile
  src_java_for_profile
  src_node_for_profile
  src_python_for_profile
  src_ruby_for_profile
  src_asdf_for_profile
  src_thef_for_profile
  src_broot_for_profile
  src_parallel_for_profile
  src_prompt_for_profile
}
# }}}

# Source the common.sh file.
scomm() {
  . "${COMMON_SOURCE_FILE}"
}

# Source entire shell environment. This function acts as a single point-of-entry
# for pulling extra definitions into a shell, and for invoking
# rvm/nvm/pyenv/jenv. It should work regardless of the shell in use.
src_all() {
  # Immediately source all function/alias files.
  __src

  # Make sure luarocks are available
  src_luarocks_module_path
  # Start SSH and GPG agents
  __src_agents
  # Take care of fixing the SSH authentication socket if logged in over SSH.
  fix_ssh_auth_sock
  # Add more tab completions to shell environment.
  add_shell_completions
  # Create a `cd' directory history file for the shell session.
  create_dir_hist_file
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
