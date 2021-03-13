#!/bin/sh

# Create an alias for a directory so that a user may easily navigate into it.
#   Usage: diralias <alias name> [<target directory>]
diralias() {
  local dirAlias="$1"
  shift
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local existingAliasValue lastByteOfFile exportString targetDir

  if [ -z "$dirAlias" ]; then
    err "No alias name given to ${FUNCNAME[0]}"
    return 1
  fi

  targetDir="${1:-$(pwd)}"
  shift

  if [ ! -f "$dirAliasFile" ]; then
    dirAliasFile="$HOME/.dirs"
  fi

  existingAliasValue="$(
    grep "export\s*${dirAlias}=" <"$dirAliasFile" |
      sed 's/^export\s*//' |
      sed 's/^.*=//' |
      sed 's/"//g'
  )"

  # If the alias already exists, get rid of the old one before adding the new
  #one.
  if [ -n "$existingAliasValue" ]; then
    sed -i "/^export\s*${dirAlias}=/d" "$dirAliasFile"
  fi

  exportString="export ${dirAlias}=\"${targetDir}\";"

  # If diralias file has no final newline character, make sure to put alias on
  # new line.
  lastByteOfFile="$(tail -c 1 "$dirAliasFile")"
  if [ "$lastByteOfFile" != "" ]; then
    echo -e "\n${exportString}" >>"$dirAliasFile"
  else
    echo "${exportString}" >>"$dirAliasFile"
  fi

  src
}

# Wrap the standard `cd` utility with out custom logic.
cd() {
  __cd "${@}"
}

# Wrapper function for the `cd` builtin that tracks directory history, and
# understands a few new sigils.
__cd() {
  local dirArg="${*}"

  # Figure out where we need to go, and then change directories.
  __do_cd "${dirArg}"

  # Finally, write the new current directory to our directory history file.
  __write_to_dir_hist "$(pwd)"
}

# Perform the tricky argument analysis to figure out where we need to go, and
# then perform the actual `cd` command.
__do_cd() {
  local dirArg="${1}"

  if echo "${dirArg}" | grep -E -q -- "-[1-9][0-9]*"; then
    __do_cd_history "${dirArg}"
  elif echo "${dirArg}" | grep -E -q -- "^\^$"; then
    __do_cd_to_git_root
  else
    __do_cd_to_dir_or_alias "${dirArg}"
  fi
}

# Perform a normal `cd` command.
__do_cd_to_dir_or_alias() {
  local dirArg="${1}"
  local aliasedDir="$(__get_directory_for_alias "${dirArg}")"

  # Replace dirArg with aliasedDir if it is defined.
  dirArg="${aliasedDir:-${dirArg}}"

  if [ ! -d "${dirArg}" ]; then
    err "${BLUE}${dirArg}${NC} is neither a directory nor a directory alias."
    return 1
  fi

  builtin cd "${dirArg}"
}

# `cd` to a directory in the history file.
__do_cd_history() {
  local dirArg="${1}"
  local numDirsToGoBack="$((${dirArg#-} + 1))"

  # Argument is a number to go back
  if __get_dir_from_hist "${numDirsToGoBack}" >>/dev/null; then
    builtin cd "$(__get_dir_from_hist "${numDirsToGoBack}")"
  fi
}

# Return to the root of the current git repository.
__do_cd_to_git_root() {
  if ! git rev-parse --is-inside-work-tree; then
    err "Not inside a git repository."
    return 1
  fi

  builtin cd "$(dirname "$(git rev-parse --git-dir)")"
}

__write_to_dir_hist() {
  local dirToWrite="${1}"
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"
  local dirHistLimit="${DIR_HIST_LIMIT:-10}"
  local numLinesInHistory="$(__get_num_dir_hist_lines)"

  # If we've hit the limit on our directory history, remove the first
  # (i.e., oldest ) line from the file.
  if [ "${numLinesInHistory}" -ge "${dirHistLimit}" ]; then
    mv "${dirHistFile}"{,.bak}
    tail -n +2 "${dirHistFile}.bak" >"${dirHistFile}"
    rm -f "${dirHistFile}.bak"
  fi

  # Write the new directory to the history file.
  printf "%s\n" "${dirToWrite}" >>"${dirHistFile}"
}

# Return the number of lines in the DIR_HIST_FILE.
__get_num_dir_hist_lines() {
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"

  wc -l "${dirHistFile}" | awk '{print $1}'
}

# Get the nth entry from the DIR_HIST_FILE.
__get_dir_from_hist() {
  local numDirsToGoBack="${1}"
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"
  local numLinesInHistory="$(__get_num_dir_hist_lines)"

  if ! echo "${numDirsToGoBack}" | grep -E -q -- "[0-9]+"; then
    err "Input must be a positive integer."
    return 1
  fi

  if [ "${numDirsToGoBack}" -gt "${numLinesInHistory}" ]; then
    err "Dir history only contains ${numLinesInHistory} directories but" \
      "entry ${numDirsToGoBack} was requested."
    return 2
  fi

  tac "${dirHistFile}" | head "-${numDirsToGoBack}" | tail -1
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
