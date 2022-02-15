#!/bin/sh

# Create an alias for a directory so that a user may easily navigate into it
# with `cd'. The `cd' wrapper function inside this dotfiles repository
# recognizes directory aliases created by this function.
#
# USAGE:
#   diralias <alias name> [<target directory>]
diralias() {
  local dirAlias
  local targetDir="$(pwd)"
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local existingAliasValue
  local lastByteOfFile
  local stringToWrite
  local shortPath

  # Get CLI args
  if [ -n "${1}" ]; then
    dirAlias="${1}"
    shift
  fi
  if [ -n "${1}" ]; then
    targetDir="${1}"
    shift
  fi

  if [ -z "${dirAlias}" ]; then
    err "No alias name given to ${FUNCNAME[0]}"
    return 1
  fi

  existingAliasValue="$(
    grep "${dirAlias}=" <"${dirAliasFile}" |
      sed 's/^export\s*//' |
      sed 's/^.*=//' |
      sed 's/"//g'
  )"

  # If the alias already exists, get rid of the old one before adding the new
  # one.
  if [ -n "${existingAliasValue}" ]; then
    sed -i "/^${dirAlias}=/d" "${dirAliasFile}"
  fi

  # Get the shortened version of the target directory path.
  shortPath="$(shortpath "${targetDir}")"

  # The diralias line to write to the DIR_ALIAS_FILE.
  stringToWrite="${dirAlias}=\"${shortPath}\""

  # Remove trailing newline from diralias file if it has one.
  perl -pi -e 'chomp if eof' "${dirAliasFile}"

  printf "\n%s\n" "${stringToWrite}" >>"${dirAliasFile}"

  # Make the new alias immediately available to the current shell.
  . "${dirAliasFile}"
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

# Reports whether a given directory is empty. Return 0 for nonempty, non-zero
# otherwise.
dirnonempty() {
  local dir="${1}"

  [ -d "${dir}" ] && [ -n "$(command ls -A "${dir}")" ]
}

# Go To Project
# gtp a fast way of bouncing around project repositories.
gtp() {
  local projectDir="${WS:-${HOME}/workspace}"
  local projectListingCommand="find '${projectDir}' -maxdepth 1 -mindepth 1 \
    -type d -printf '%f\n' |
    sort -u"
  local selectedProject

  # Prioritized list of fuzzy search tools.
  if [ -x "$(command -v fzf)" ]; then
    selectedProject="$(eval "${projectListingCommand}" | fzf)"
  elif [ -x "$(command -v fzy)" ]; then
    selectedProject="$(eval "${projectListingCommand}" | fzy)"
  else
    err "No fuzzy searching cli tool found"
    return 1
  fi

  if [ -z "${selectedProject}" ]; then
    return 0
  elif [ ! -d "${projectDir}/${selectedProject}" ]; then
    err "${BLUE}${selectedProject}${NC} is not a project directory"
    return 2
  fi

  cd "${projectDir}/${selectedProject}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
