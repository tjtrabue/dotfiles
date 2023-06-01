#!/bin/sh

# Wrap the standard `cd` utility with custom logic to handle arguments like '-1'
# to return to the previous directory, or '@' to return to the root of a Git
# repository.
cd() {
  # Print help message if requested.
  if echo "${1}" | grep -q -E -- '^\s*(-h)|(--help)\s*$'; then
    __cd_help
    return 0
  fi

  __cd "${*}"
}

# cd to project
# `cdp` a fast way of bouncing around project repositories.
cdp() {
  local projectDir="${1}"
  local projectListingCommand="find \"\${projectDir}\" -maxdepth 1 -mindepth 1 \
    -type d -printf '%f\n' |
    sort -u"
  local selectedProject

  if [ -z "${projectDir}" ]; then
    projectDir="${WS:-${HOME}/workspace}"
  fi

  if [ ! -d "${projectDir}" ]; then
    err "${BLUE}${projectDir}${NC} is not a directory"
    return 1
  elif [ -z "$(ls -1 "${projectDir}")" ]; then
    err "No projects found in ${BLUE}${projectDir}${NC}"
    return 2
  fi

  # Prioritized list of fuzzy search tools.
  if [ -x "$(command -v fzf)" ]; then
    selectedProject="$(eval "${projectListingCommand}" | fzf)"
  elif [ -x "$(command -v fzy)" ]; then
    selectedProject="$(eval "${projectListingCommand}" | fzy)"
  else
    err "No fuzzy searching cli tool found"
    return 3
  fi

  if [ -z "${selectedProject}" ]; then
    return 0
  elif [ ! -d "${projectDir}/${selectedProject}" ]; then
    err "${BLUE}${projectDir}/${selectedProject}${NC} is not a directory"
    return 4
  fi

  cd "${projectDir}/${selectedProject}"
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

__cd_help() {
  command cat <<EOF
cd - Special 'cd' wrapper function defined in ${DOTFILES_HOME}

USAGE:
  cd DIRECTORY
  cd DIRALIAS
  cd @
  cd -N

DESCRIPTION OF ARGUMENTS

  DIRECTORY:
  This cd wrapper function can, of course, handle any standard directory
  argument that the builtin cd accepts.

  DIRALIAS:
  If the argument to cd is a directory alias defined in ${DIR_ALIAS_FILE},
  change into the directory pointed at by DIRALIAS.

  @:
  If the argument to cd is '@', and the user is in a Git repository, change into
  the root directory of the repository.

  -N:
  If the argument to cd is '-N', where N is a positive integer, return to the
  Nth directory visited in the history file.
EOF
}

# Perform the tricky argument analysis to figure out where we need to go, and
# then perform the actual `cd` command.
__do_cd() {
  local dirArg="${1}"

  if echo "${dirArg}" | grep -E -q -- '^-[1-9][0-9]*$'; then
    log_debug "cd'ing back into history: ${dirArg}"
    __do_cd_history "${dirArg}"
  elif echo "${dirArg}" | grep -E -q -- '^@$'; then
    # Use '@' to cd to the root of the current git repository.
    log_debug "cd'ing to project root: ${dirArg}"
    __do_cd_to_project_root
  else
    log_debug "cd'ing to directory or alias: ${dirArg}"
    __do_cd_to_dir_or_alias "${dirArg}"
  fi
}

# Perform a normal `cd` command.
__do_cd_to_dir_or_alias() {
  local dirArg="${1}"
  local aliasedDir="$(__get_directory_for_alias "${dirArg}")"

  # Replace dirArg with aliasedDir if it is defined.
  dirArg="${aliasedDir:-${dirArg}}"

  if [ -n "${dirArg}" ] && [ ! -d "${dirArg}" ]; then
    err "${BLUE}${dirArg}${NC} is neither a directory nor a directory alias."
    return 1
  fi

  # For some reason using `builtin cd ` with an empty argument doesn't cd back
  # to the user's $HOME directory, so we have to explicitly set an empty dirArg
  # to $HOME.
  dirArg="${dirArg:-${HOME}}"

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

# Return to the root of the current project directory tree.
# Usually, this amounts to returning to the root of the current Git/VCS
# repository.
__do_cd_to_project_root() {
  if isrepo; then
    __do_cd_to_vcs_project_root
  else
    err "Could not determine project root"
    return 1
  fi
}

# Return to top level of the current version control repository.
__do_cd_to_vcs_project_root() {
  if isgitrepo; then
    __do_cd_to_git_project_root
  else
    err "Could not determine VCS type"
    return 1
  fi
}

# Return to top level of the current Git repository.
__do_cd_to_git_project_root() {
  if ! isgitrepo; then
    err "Not inside a VCS repository."
    return 1
  fi

  builtin cd "$(dirname "$(git rev-parse --git-dir)")"
}

# Write the current directory to the history file.
__write_to_dir_hist() {
  local dirToWrite="${1}"
  local dirHistFile="${DIR_HIST_FILE:-${HOME}/.dir_history}"
  local dirHistLimit="${DIR_HIST_LIMIT:-10}"
  local numLinesInHistory="$(__get_num_dir_hist_lines)"
  local latestDirFromHist="$(__get_dir_from_hist 1)"

  # If we've hit the limit on our directory history, remove the first
  # (i.e., oldest ) line from the file.
  if [ "${numLinesInHistory}" -ge "${dirHistLimit}" ]; then
    mv "${dirHistFile}"{,.bak}
    tail -n +2 "${dirHistFile}.bak" >"${dirHistFile}"
    rm -f "${dirHistFile}.bak"
  fi

  if
    [ "$(eval "echo ${dirToWrite}")" != "$(eval "echo ${latestDirFromHist}")" ]
  then
    # Write the new directory to the history file.
    printf "%s\n" "${dirToWrite}" >>"${dirHistFile}"
  fi
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

  while IFS="" read -r line || [ -n "${line}" ]; do
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

# Create a new file to hold `cd' history under the /tmp/ directory, and store
# the newly created file path in DIR_HIST_FILE environment variable. This file
# will hold the number of previous `cd' directories equal to the DIR_HIST_LIMIT
# environment variable.
create_dir_hist_file() {
  local numDirHistFilesToKeep=30

  if [ -f "${DIR_HIST_FILE}" ]; then
    warn "${CYAN}DIR_HIST_FILE${NC} variable already set to: " \
      "${BLUE}${DIR_HIST_FILE}${NC}." \
      "Not creating new file."
    return 0
  fi

  {
    export DIR_HIST_FILE="$(mktemp -t dir_hist.XXXXXXXXXX)" &&
      printf "%s\n" "$(pwd)" >>"${DIR_HIST_FILE}" &&
      log_info "Created ${CYAN}DIR_HIST_FILE${NC}:" \
        "${BLUE}${DIR_HIST_FILE}${NC}" &&
      __delete_old_dir_hist_files "${numDirHistFilesToKeep}"
  } || {
    err "Could not create ${CYAN}DIR_HIST_FILE${NC}"
    return 1
  }
}

# Keep only the n most recent directory history files. If no number is provided,
# the default number to keep is 30.
__delete_old_dir_hist_files() {
  local numDirHistFilesToKeep="${1:-30}"
  local dirHistFilePrefix="${DIR_HIST_FILE_PREFIX:-dir_hist}"

  log_info "Deleting old dir hist files (keeping" \
    "${GREEN}${numDirHistFilesToKeep}${NC} most recent files)"

  command ls -1tr "/tmp/${dirHistFilePrefix}"* |
    head -n -"${numDirHistFilesToKeep}" |
    xargs -d '\n' rm -f --
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
