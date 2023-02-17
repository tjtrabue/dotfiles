#!/bin/sh

# Functions in this file handle installing generic tooling from the web.

# Checks to see if a generic tool is installed on the local file system.
# Takes the tool's command and home directory as its arguments.
# Returns 0 if the tool is installed. Returns non-zero otherwise.
tool_installed() {
  local toolCmd="$1"
  local toolHomeDir="$2"

  if [ -z "${toolCmd}" ]; then
    err "No tool command provided."
    return 1
  elif [ -z "${toolHomeDir}" ]; then
    err "No tool home directory provided."
    return 2
  fi

  if [ "$(command -v "${toolCmd}")" = "" ] && [ ! -d "${toolHomeDir}" ]; then
    return 3
  fi
  return 0
}

# Install a tool, such as pyenv, jenv, sdkman, etc. from a remote url.
# This function takes was inspired by observing a similar pattern appearing in
# many different "install_x" type functions. Each of those functions operated
# on a tool's name, home directory, and a url. Thus, it was easy to capture all
# of their common logic in a single function.
install_tool_from_url_and_script() {
  local toolCmd="$1"
  local toolHomeDir="$2"
  local installerUrl="$3"

  __install_tool_generic "${toolCmd}" "${toolHomeDir}" \
    "curl -sSL \"${installerUrl}\" | bash"
}

# This function is similar to install_tool_from_url_and_script, only it clones
# a git repository instead of running an installer script through bash.
install_tool_from_git() {
  local toolCmd="$1"
  local toolHomeDir="$2"
  local gitRepoUrl="$3"

  __install_tool_generic "${toolCmd}" "${toolHomeDir}" \
    "git clone \"${gitRepoUrl}\" ${toolHomeDir}"
}

# Pull updates for the git-installed tool.
update_tool_from_git() {
  local toolHomeDir="$1"

  if [ ! -d "${toolHomeDir}" ]; then
    err "Tool home directory not found at: ${BLUE}${toolCmd}${NC}"
    return 1
  fi

  log_info "Updating tool at: ${BLUE}${toolHomeDir}${NC}"
  git -C "${toolHomeDir}" reset --hard
  git -C "${toolHomeDir}" pull
}

# Generic code for installing from an installer URL or a Git repository.
__install_tool_generic() {
  local toolCmd="$1"
  local toolHomeDir="$2"
  local installCmdString="$3"

  if ! tool_installed "${toolCmd}" "${toolHomeDir}"; then
    eval "${installCmdString}"
  else
    warn "${toolCmd} is already installed. Check ${toolHomeDir}"
  fi
}

# Create a command line of package names from a file ready to use in a package
# manager install command.
build_package_cmd_line_from_file() {
  local packageFile="${1}"

  if [ -z "${packageFile}" ]; then
    err "No package file provided"
    return 1
  elif [ ! -f "${packageFile}" ]; then
    err "Cannot create command line from ${BLUE}${packageFile}${NC};" \
      "no such file"
    return 2
  fi

  log_info "Building command line to install packages from:" \
    "${BLUE}${packageFile}${NC}"

  grep -E -v '^\s*#' "${packageFile}" |
    rmduplines |
    tr '\n' ' ' |
    sed -E -e 's/^\s*//' -e 's/\s*$//'
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
