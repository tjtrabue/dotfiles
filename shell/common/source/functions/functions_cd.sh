#!/bin/sh

# Wrapper function for the `cd` builtin that tracks directory history, and
# understands a few new sigils.
__cd() {
  local dirArg="${1}"
  local finalDirArg

  command cd "${finalDirArg}"
}

__write_to_dir_hist() {
  local dirToWrite="${1}"
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"
}

__translate_dir_history() {
  local numDirsToGoBack="${1}"
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"
  local numLinesInHistory="$(wc -l "${dirHistFile}" | awk '{print $1}')"
  local index=1
  local line

  if ! echo "${numDirsToGoBack}" | grep -E -q "[0-9]+"; then
    err "Input must be a positive integer."
    return 1
  fi

  if [ "${numDirsToGoBack}" -gt "${numLinesInHistory}" ]; then
    err "Cannot go back ${numDirsToGoBack} directories because there are only" \
      "${numLinesInHistory} lines in the directory history file."
    return 2
  fi

  for line in $(tac "${dirHistFile}"); do
    if [ "${numDirsToGoBack}" -eq "${index}" ]; then
      echo "${line}"
      break
    fi
    ((index += 1))
  done
}

# Retrieve the directory alias
__get_directory_for_alias() {
  local dirAlias="${1}"
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local line
  local currentAlias

  while read -r line || [ -n "${line}" ]; do
    # Ignore empty or commented lines.
    if echo "${line}" | grep -E -q -e "^#" -e "^$"; then
      continue
    fi
    # Only look at the alias name, so remove the '=' and everything that comes
    # after.
    currentAlias="${line%=*}"
    # Also take off the 'export ' prefix, if it exists.
    currentAlias="${currentAlias#export }"
    if [ "${dirAlias}" = "${currentAlias}" ]; then
      eval "echo -e \"\$${dirAlias}\""
    fi
  done <"${dirAliasFile}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
