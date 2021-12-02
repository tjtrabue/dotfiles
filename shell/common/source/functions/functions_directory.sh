#!/bin/sh

# Create an alias for a directory so that a user may easily navigate into it
# with `cd'. The `cd' wrapper function inside this dotfiles repository
# recognizes directory aliases created by this function.
#
# USAGE:
#   diralias <alias name> [<target directory>]
diralias() {
  local dirAlias="$1"
  shift
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local existingAliasValue
  local lastByteOfFile
  local stringToWrite
  local targetDir

  if [ -z "${dirAlias}" ]; then
    err "No alias name given to ${FUNCNAME[0]}"
    return 1
  fi

  targetDir="${1:-$(pwd)}"

  existingAliasValue="$(
    grep "${dirAlias}=" <"${dirAliasFile}" |
      sed 's/^export\s*//' |
      sed 's/^.*=//' |
      sed 's/"//g'
  )"

  # If the alias already exists, get rid of the old one before adding the new
  #one.
  if [ -n "${existingAliasValue}" ]; then
    sed -i "/^${dirAlias}=/d" "${dirAliasFile}"
  fi

  # The diralias line to write to the DIR_ALIAS_FILE.
  stringToWrite="${dirAlias}=\"${targetDir}\""

  # Remove trailing newline from diralias file if it has one.
  perl -pi -e 'chomp if eof' "${dirAliasFile}"

  printf "\n%s\n" "${stringToWrite}" >>"${dirAliasFile}"

  # Make the new alias immediately available to the current shell.
  . "${dirAliasFile}"
}

currdir() {
  dirname "$0"
}

# Alias function for invoking a CLI directory navigation tool the user
# specifies.
dirnav() {
  local dirNavTool="ranger-cd"

  if [ -x "$(command -v broot)" ]; then
    # Use broot if possible, since its faster than ranger.
    dirNavTool="br"
  fi

  eval "${dirNavTool}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
