#!/bin/sh

# Functions in this file handle installing generic tooling from the web.

# Checks to see if a generic tool is installed on the local file system.
# Takes the tool's command and home directory as its arguments.
# Returns 0 if the tool is installed. Returns non-zero otherwise.
__tool_installed() {
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

# vim:foldenable:foldmethod=indent:foldnestmax=1
