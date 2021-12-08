#!/bin/sh
# Contains custom Git functions that can be run in the terminal.

# General {{{

# Determines whether the given directory is in a Git repository.
# If no repository directory is provided, defaults to the current dir.
# Return 0 if the directory is a Git repo. Return non-zero otherwise.
isgitrepo() {
  local repoDir="${1:-$(git rev-parse --show-toplevel 2>/dev/null)}"

  git -C "${repoDir}" rev-parse >>/dev/null 2>&1
}

# Retrieves the Git URL for the current repository.
remoteurl() {
  git remote -v | egrep '^origin.*push' | awk '{print $2}'
}

# Return the name of the default configured remote repository for the given Git
# repository. If no repository path is provided, the current working directory
# will be used.
defaultremote() {
  local gitRepo="${1:-$(git rev-parse --show-toplevel 2>/dev/null)}"
  local defaultRemote

  if ! isgitrepo "${gitRepo}"; then
    err "${BLUE}${gitRepo}${NC} is not a Git repository."
    return 1
  fi

  defaultRemote="$(git -C "${gitRepo}" remote 2>/dev/null)"

  if [ -z "${defaultRemote}" ]; then
    # If we can't intelligently determine the default remote name, assume the
    # standard remote name of "origin"
    defaultRemote="origin"
  fi

  echo "${defaultRemote}"
}

# Returns the default remote branch name for a given repository. If no
# repository is provided, defaults to the current directory.
defaultbranch() {
  local gitRepo="${1:-$(git rev-parse --show-toplevel 2>/dev/null)}"
  local defaultRemote="$(defaultremote "${gitRepo}")"

  if ! isgitrepo "${gitRepo}"; then
    err "${BLUE}${gitRepo}${NC} is not a Git repository."
    return 1
  fi

  git -C "${gitRepo}" remote show "${defaultRemote}" 2>/dev/null |
    sed -n '/HEAD branch/s/.*: //p'
}

# Retrieve the name of the currently checked-out Git ref.
currentref() {
  local gitRepo="${1:-$(git rev-parse --show-toplevel 2>/dev/null)}"

  git -C "${gitRepo}" rev-parse --abbrev-ref HEAD 2>/dev/null
}

# Report the name of the given (or current) Git repository.
reponame() {
  local gitRepo="${1:-$(git rev-parse --show-toplevel 2>/dev/null)}"
  basename "${gitRepo}"
}

# Opens the commit message for the current repo in the configured editor.
emsg() {
  edit "$(git rev-parse --show-toplevel)/.git/COMMIT_EDITMSG"
}
# }}}

# Branching/switching branches {{{

# Switch to a Git branch/commit/tag.
# Supports history, so we can return to the previously visited ref using
#   sw -1
# or even further with, for instance:
#   sw -4
sw() {
  local arg="${1}"
  local ref
  # We want to write the current branch/commit to history after we switch.
  local currentRef="$(currentref)"

  if [ -z "${arg}" ]; then
    err "No ref provided to switch"
    return 1
  fi

  __init_sw_config

  if echo "${arg}" | grep -q '^-[0-9]$'; then
    # Get nth line from history file if arg is of the form'-<n>' where n is an
    # integer.
    ref="$(__get_sw_numeric_ref_from_hist_file "${arg}")"
  else
    # Otherwise, assume arg is the name of the ref we want to checkout.
    ref="${arg}"
  fi

  if ! verifyref "${ref}"; then
    err "${CYAN}${ref}${NC} is not a valid Git ref"
    return 2
  fi

  if ! __checkout_local_or_remote_branch "${ref}"; then
    err "Could not switch to ref: ${CYAN}${ref}${NC}"
    return 3
  fi

  if [ "${ref}" != "${currentRef}" ]; then
    __save_ref_to_sw_hist_for_repo "${currentRef}"
  fi
}

# Initialize sw configuration necessary for its operations, such as making its
# configuration directories.
__init_sw_config() {
  local dirsToCreate=(
    "${SW_CONFIG_DIR}"
    "${SW_HISTORY_DIR}"
  )
  local d

  for d in "${dirsToCreate[@]}"; do
    [ ! -d "${d}" ] && mkdir -p "${d}"
  done
}

# Get nth line from history file if arg is of the form'-<n>' where n is an
# integer.
__get_sw_numeric_ref_from_hist_file() {
  local arg="${1}"
  local histFile="$(__get_sw_hist_file_name_for_repo)"

  if [ ! -f "${histFile}" ]; then
    err "Could not find sw history file for ${BLUE}$(reponame)${NC}"
    return 1
  fi

  if ! echo "${arg}" | grep -q '^-[0-9]$'; then
    err "arg is not of the form -<n>, where n is an integer"
    return 2
  fi

  sed "${arg#-}q;d" "${histFile}"
}

# Intelligently determine whether a local copy of a remote branch exists. If it
# does not, we must first create a local copy and instruct the branch to track
# its remote counterpart. If the local copy already exists, we have only to
# switch to it.
__checkout_local_or_remote_branch() {
  local remoteBranch="${1}"
  local localBranch="$(basename "${remoteBranch}" 2>/dev/null)"
  local currentRef="$(currentref)"
  local defaultRemote="$(defaultremote)"

  if [ -z "${remoteBranch}" ]; then
    err "No branch name provided"
    return 1
  elif [ "${localBranch}" = "${currentRef}" ]; then
    warn "HEAD is already set to ref: ${CYAN}${localBranch}${NC}"
    return 0
  fi

  if ! verifyref "${localBranch}"; then
    git checkout -t "${remoteBranch}"
  else
    git branch -u "${defaultRemote}/${localBranch}" "${localBranch}" 2>/dev/null
    git checkout "${localBranch}"
  fi
}

# Persist most recent ref passed to `sw' to the history file for that Git
# repository.
__save_ref_to_sw_hist_for_repo() {
  local ref="${1}"
  local histFile
  local histTempFile
  local numRefsToSave="10"

  if [ -z "${ref}" ]; then
    err "No ref provided to save to sw history file"
    return 1
  fi

  if [ ! -d "${SW_HISTORY_DIR}" ]; then
    warn "sw history directory does not exist; not writing ref"
    return 0
  fi

  histFile="$(__get_sw_hist_file_name_for_repo)"
  histTempFile="${histFile}.tmp"

  if [ -f "${histFile}" ]; then
    log_debug "Writing sw ref ${CYAN}${ref}${NC} to history file:" \
      "${GREEN}${histFile}${NC}"

    echo "${ref}" |
      cat - "${histFile}" |
      rmduplines |
      head -n "${numRefsToSave}" >"${histTempFile}"

    mv "${histTempFile}" "${histFile}"
  else
    log_debug "Creating new ref history file with entry: ${CYAN}${ref}${NC}"
    echo "${ref}" >"${histFile}"
  fi
}

# Try to make the sw history file for each Git repository as unique as possible
# by appending a unique identifier to the end of each history file name.
# This avoids collisions where multiple repositories have the same base
# directory name, but are located in different parent directories.
__get_sw_hist_file_name_for_repo() {
  local repoPath="$(git rev-parse --show-toplevel 2>/dev/null)"
  local repoName="$(basename "${repoPath}")"
  local histFile="${SW_HISTORY_DIR}/${repoName}"

  # Append a hash digest of the complete directory path to the Git repo, if
  # possible.
  if [ -x "$(command -v sha256sum)" ]; then
    histFile="${histFile}-$(echo -n "${repoPath}" | sha256sum | awk '{print $1}')"
  elif [ -x "$(command -v sha1sum)" ]; then
    histFile="${histFile}-$(echo -n "${repoPath}" | sha1sum | awk '{print $1}')"
  elif [ -x "$(command -v md5sum)" ]; then
    histFile="${histFile}-$(echo -n "${repoPath}" | md5sum | awk '{print $1}')"
  fi

  histFile="${histFile}.txt"
  echo "${histFile}"
}

# Interactive branch switching using fuzzy search program.
swi() {
  local branch
  local branchListingCommand="git branch -a --format '%(HEAD)%(refname:short)' |
    grep -v -e '^\s*\*' -e 'HEAD' |
    sed -e 's/^\s*//' -e 's/\s*\$//' |
    awk '{print \$1}'"

  # Prioritized list of fuzzy search tools used to select the branch.
  if [ -x "$(command -v fzf)" ]; then
    branch="$(eval "${branchListingCommand}" | fzf)"
  elif [ -x "$(command -v fzy)" ]; then
    branch="$(eval "${branchListingCommand}" | fzy)"
  else
    branch="$(eval "${branchListingCommand}" | __swi_default_list_branches)"
  fi

  if [ -n "${branch}" ]; then
    sw "${branch}"
  fi
}

# Interactive switching program used when no fuzzy finder programs can be found.
__swi_default_list_branches() {
  local branches=("$@")
  local selection
  local b
  local i=1
  local maxValue

  if [ "${#branches[@]}" -eq 0 ]; then
    branches=()
    while IFS="" read -r b || [ -n "${b}" ]; do
      branches+=("${b}")
    done </dev/stdin
  fi

  printf "%s\n" "Select a branch index to switch:" 1>&2
  for b in "${branches[@]}"; do
    printf "[%-2d] %s\n" "${i}" "${b}" 1>&2
    ((i += 1))
  done

  read -r selection </dev/tty

  if [ "${selection}" -le 0 ] || [ "${selection}" -ge "${i}" ]; then
    err "Invalid branch index"
    return 1
  elif [ -z "${selection}" ]; then
    err "No branch index read"
    return 2
  fi

  echo "${branches[${selection}]}"
}

# Lists all branch names, local and remote, for the given repo.
gbl() {
  git branch -a --color=never --format="%(refname:lstrip=-1)" |
    grep -v '^HEAD$' | sort -u
}

# Pabst Blue Ribbon...
# Nah, it's "project branch".
pbr() {
  local taskNumber
  local description
  local projectIdentifier
  local projectBranchName

  if [ -z "${taskNumber}" ] && [ -n "${1}" ]; then
    taskNumber="${1}"
    shift
  fi

  if [ -z "${description}" ] && [ -n "${1}" ]; then
    description="${1}"
    shift
  fi

  if [ -z "${projectIdentifier}" ] && [ -n "${1}" ]; then
    projectIdentifier="${1}"
    shift
  fi

  while ! __validate_task_number "${taskNumber}"; do
    command cat <<EOF
Enter task number:
EOF
    read -r taskNumber
  done

  if [ -z "${projectIdentifier}" ]; then
    __src_project_vars_for_git_project
    projectIdentifier="${PROJECT_IDENTIFIER}"
  fi
  while [ -z "${projectIdentifier}" ]; do
    command cat <<EOF
Enter project ID:
EOF
    read -r projectIdentifier
  done

  while [ -z "${description}" ]; do
    command cat <<EOF
Enter branch description:
EOF
    read -r description
  done

  projectBranchName="$(__construct_project_branch "${taskNumber}" \
    "${description}" \
    "${projectIdentifier}")"

  git checkout -b "${projectBranchName}"
}

__construct_project_branch() {
  local taskNumber="${1}"
  local description="${2}"
  local projectIdentifier="${3}"
  local formatString="%s-%s.%s"
  local formattedDescription="$(echo "${description}" |
    sed -E 's/\s+/./g')"

  printf "${formatString}" \
    "${projectIdentifier}" \
    "${taskNumber}" \
    "${formattedDescription}"
}
# }}}

# Committing {{{

# Shortcut for `git commit -m '<msg>'`. No need to put quote marks around your
# commit message. This function handles that bit for you.
gcm() {
  local message="${*}"

  if git diff --cached --exit-code --quiet; then
    err "No files added to the index. Use 'git add' to add them."
    return 1
  fi

  while [ -z "${message}" ]; do
    echoe "Enter commit message:"
    read -r message
  done

  if [ -z "${message}" ]; then
    err "No commit message provided."
    return 2
  fi

  git commit -m "${message}"
}

# Commit for project.
pcm() {
  local taskNumber
  local commitMsg
  local projectIdentifier
  local commitMsgFormat="${PROJECT_MSG_STYLE}"
  local finalCommitMsg
  local OPTIND
  local o

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  while getopts ":hm:n:p:f:" o; do
    case "${o}" in
    h)
      __pcm_help
      return 0
      ;;
    m)
      commitMsg="${OPTARG}"
      ;;
    n)
      taskNumber="${OPTARG}"
      ;;
    p)
      projectIdentifier="${OPTARG}"
      ;;
    f)
      commitMsgFormat="${OPTARG}"
      ;;
    *)
      err "Unknown operand"
      __pcm_usage
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  if [ -z "${taskNumber}" ] && [ -n "${1}" ]; then
    taskNumber="${1}"
    shift
  fi
  # If we did not get the task number as a positional parameter, try to parse
  # it from the current branch name.
  if [ -z "${taskNumber}" ]; then
    taskNumber="$(__parse_branch_for_task_number "$(currentref)")"
  fi

  if [ -z "${commitMsg}" ] && [ -n "${1}" ]; then
    commitMsg="${1}"
    shift
  fi

  if [ -z "${projectIdentifier}" ] && [ -n "${1}" ]; then
    projectIdentifier="${1}"
    shift
  fi
  # If we did not get the project ID as a positional parameter, try to parse it
  # from the current branch name.
  if [ -z "${projectIdentifier}" ]; then
    projectIdentifier="$(__parse_branch_for_project_id "$(currentref)")"
  fi

  # Read task number interactively if it could not be deduced elsewhere.
  while ! __validate_task_number "${taskNumber}"; do
    command cat <<EOF
Enter task number:
EOF
    read -r taskNumber
  done

  # Task number validation
  if ! __validate_task_number "${taskNumber}"; then
    err "Task number must be a string of integers"
    return 2
  fi

  # Attempt to get project identifier from .git_project.sh file, and then
  # finally from the parent environment if that file is not set.
  if [ -z "${projectIdentifier}" ]; then
    __src_project_vars_for_git_project
    projectIdentifier="${PROJECT_IDENTIFIER}"
  fi

  while [ -z "${projectIdentifier}" ]; do
    command cat <<EOF
Enter project ID string:
EOF
    read -r projectIdentifier
  done

  # Read commit message interactively if it was not supplied on the command
  # line.
  while [ -z "${commitMsg}" ]; do
    command cat <<EOF
Enter commit message:
EOF
    read -r commitMsg
  done

  finalCommitMsg="$(
    __construct_project_commit_msg \
      "${taskNumber}" \
      "${commitMsg}" \
      "${projectIdentifier}" \
      "${commitMsgFormat}"
  )"

  if ! __validate_project_commit_msg "${finalCommitMsg}"; then
    err "Commit message regex validation failed"
    return 5
  fi

  git commit -m "${finalCommitMsg}"
}

__pcm_usage() {
  command cat <<EOF
USAGE:
  pcm [-h | -m COMMIT_MSG | -n TASK_NUMBER | -p PROJECT_IDENTIFIER |
       -f MSG_FORMAT]

  pcm [TASK_NUMBER] [COMMIT_MSG] [PROJECT_IDENTIFIER]
EOF
}

__pcm_help() {
  __pcm_usage
  command cat <<EOF

OPTIONS:
  -h: Print the help message (this message) and exit.

  -m COMMIT_MSG: Supply the commit message as an optional argument.
                 If this option is omitted, the user may supply the commit
                 message as a positional parameter. Otherwise, the user will be
                 prompted to enter the it interactively.

  -n TASK_NUMBER: Supply the task number. If this option is omitted, the user
                  may supply the task number as a positional parameter.
                  Otherwise, the user will be prompted to enter the it
                  interactively.

  -p PROJECT_IDENTIFIER: Supply the project ID string. If this option is
                         omitted, the user may supply the project ID as a
                         positional parameter. If no other value for
                         this parameter is supplied, its value will be read from
                         the PROJECT_IDENTIFIER environment variable, and,
                         failing that, the user will be prompted for the value
                         of the project ID interactively.

  -f MSG_FORMAT: The format for the commit message. The value of this variable
                 can be any value that is valid for PROJECT_MSG_STYLE. See the
                 section on PROJECT_MSG_STYLE below for more details.

ENVIRONMENT VARIABLES:
  Each of these environment variables may be placed in a per-project file named
  '${PROJECT_ENV_FILE_NAME}' located in the project's root directory.

  PROJECT_IDENTIFIER: The project ID string to use for the current project. It's
                      value should look like 'PROJ'.

  PROJECT_MSG_STYLE: Should be one of the strings listed in the examples section
                     below. This variable determines how the commit message
                     string will be formatted. If this environment variable is
                     not set, 'colon' will be used by default.
                     Examples:
                       colon -> "PROJ-1234: This is the commit message"
                       braces -> "[PROJ-1234] This is the commit message"
                       nopunct -> "PROJ-1234 This is the commit message"
EOF
}

# Try to get the project ID string from the current branch name.
# For example:
#   PROJ-1234 -> PROJ
__parse_branch_for_project_id() {
  local branchName="${1}"

  if __validate_project_branch "${branchName}"; then
    echo "${branchName}" | grep -E --color=never -o '^[A-Z]+'
  fi
}

# Try to get the task number from the current branch name.
# For example:
#   PROJ-1234 -> 1234
__parse_branch_for_task_number() {
  local branchName="${1}"

  if __validate_project_branch "${branchName}"; then
    echo "${branchName}" | grep -E --color=never -o '[0-9]+'
  fi
}

# Return 0 if input branch name is of the form 'PROJ-1234...'.
# Return an error code, otherwise.
__validate_project_branch() {
  local branchName="${1}"

  echo "${branchName}" | grep -E -q -o '[A-Z]+-[0-9]+.*'
}

# Prints commit messages in a variety of established formats, determined by the
# PROJECT_MSG_STYLE environment variable.
# See 'pcm -h' for more information on acceptable formats.
__construct_project_commit_msg() {
  local taskNumber="${1}"
  local commitMsg="${2}"
  local projectIdentifier="${3}"
  local commitMsgFormat="${4:-${PROJECT_MSG_STYLE}}"
  local formatString

  if [ -z "${commitMsgFormat}" ]; then
    commitMsgFormat="colon"
  fi

  case "${commitMsgFormat}" in
  'braces')
    formatString='[%s-%s] %s'
    ;;
  'nopunct')
    formatString='%s-%s %s'
    ;;
  *)
    formatString='%s-%s: %s'
    ;;
  esac

  printf "${formatString}" "${projectIdentifier}" "${taskNumber}" "${commitMsg}"
}

# Attempt to get project environment variables from the .git_project.sh file at
# the root of the project.
__src_project_vars_for_git_project() {
  local projectRoot="$(git rev-parse --show-toplevel)"
  local gitProjectShellFile="${projectRoot}/${PROJECT_ENV_FILE_NAME}"

  if [ -f "${gitProjectShellFile}" ]; then
    . "${gitProjectShellFile}"
  fi
}

__validate_task_number() {
  local taskNumber="${1}"

  if ! echo "${taskNumber}" | grep -E -q '^[0-9]+$'; then
    return 1
  fi
}

__validate_project_commit_msg() {
  local commitMsg="${1}"

  if ! echo "${commitMsg}" | grep -E -q \
    -e '^[A-Z]+-[0-9]+:\s+.*$' \
    -e '^[A-Z]+-[0-9]+\s+.*$' \
    -e '^\[[A-Z]+-[0-9]+\]\s+.*$'; then
    return 1
  fi
}
# }}}

# Diff {{{

# Show last diff for a given file
lastchange() {
  local file="$1"

  if [ -z "${file}" ]; then
    err "No filename provided."
    return 1
  fi

  if [ ! -f "${file}" ]; then
    err "Path ${BLUE}${file}${NC} is not a file"
    return 2
  fi

  git log -p -1 "${file}"
}

# An extended diff function leveraging powerful previewer tools such as bat.
gdd() {
  if [ -x "$(command -v bat)" ]; then
    git diff --name-only --diff-filter=d | xargs bat --diff
  else
    # Revert to plain old `git diff` if no fancy tools are available.
    git diff
  fi
}
# }}}

# Macro functions {{{

# These functions run sequences of Git commands in order to make the developer's
# life easer.

# Run my favorite Git command sequence:
#   `git add -A`
#   `git commit -m <msg>`
#   `git push origin HEAD`
ggg() {
  local commitMsg="${*}"

  git add -A

  # Try to commit changes, and return an error if changes could not be
  # committed.
  if ! gcm "${commitMsg}"; then
    return 1
  fi

  # Make use of our handy `git push` wrapper.
  gpoh
}
# }}}

# Pushing {{{

# Alias function for "git push origin HEAD".
gpoh() {
  local currentRef="$(currentref)"
  local defaultRemote="$(defaultremote)"

  # Make sure we set the current branch to track the its remote counterpart.
  git push -u "${defaultRemote}" "${currentRef}"
}
# }}}

# Rebasing/squashing {{{

# Squash all commits on the current feature branch down to a single commit, and
# rebase onto a given base branch.
#
# Note that in Git branches are simply pointers to commits, so there is no such
# thing as a "parent" branch of another branch. When one desires to squash all
# commits on a branch, the squashing is done relative to a specified "ancestor"
# branch. This ancestor is not truly a parent of the current branch, only
# nominally so. Thus, one could specify another feature branch as the base
# branch, and the current branch would rebase on top of the other feature
# branch. Please be careful before using this function.
squashfor() {
  local baseBranch="${1:-develop}"
  local currentBranch
  local commitMsg
  local response

  currentBranch="$(currentref)"

  if [ "${currentBranch}" = "master" ] ||
    [ "${currentBranch}" = "develop" ]; then
    err "Not squashing commits on protected branch: ${currentBranch}"
    return 1
  fi

  log_info "Using base branch: ${baseBranch}"
  log_info "Branch to squash: ${currentBranch}"

  while [ -z "${commitMsg}" ]; do
    echoe "Please enter a commit message for the squashed commits:"
    read -r commitMsg
  done

  while ! echo "${response}" | grep -q "[YyNn]"; do
    command cat <<EOF
Do you wish to squash all commits on ${curentBranch} with message:
'${commitMsg}'? [y/n]
EOF
    read -r response
  done

  if echo "${response}" | grep -q "[Nn]"; then
    echoe "Aborting"
    return 2
  fi

  git reset "$(git merge-base "${baseBranch}" "${currentBranch}")"
  git add -A
  git commit -m "${commitMsg}"
}

# }}}

# Reverting/resetting {{{

# Go nuclear: get rid of ALL uncommitted changes to the current working tree of
# the provided Git repository (or the repo the current directory belongs to if
# no repo is given).
#
# WARNING: Be VERY careful before you use this function!!!! You cannot undo the
# changes it makes!
totalgitreset() {
  local repo
  local OPTIND
  local o
  local force=false
  local response

  # Usage function for when user enters "-h" option.
  totalgitreset_usage() {
    echoe "USAGE: totalgitreset [-fh] [REPO]"
  }

  while getopts ":fh" o; do
    case "${o}" in
    f)
      force=true
      ;;
    h)
      totalgitreset_usage
      return 0
      ;;
    *)
      err "Unknown operand"
      totalgitreset_usage
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  repo="${1:-$(git rev-parse --show-toplevel)}"

  if ! "${force}"; then
    while ! echo "${response}" | grep -E -q "^[YyNn]$"; do
      echoe "WARNING: Do you really want to revert ALL changes to repo" \
        "${BLUE}$(basename "${repo}")${NC}? You cannot undo these changes" \
        "[y/n]"
      read -r response
    done
    if echo "${response}" | grep -E -q "^[Nn]$"; then
      echoe "Aborted by user."
      return 2
    fi
  fi

  git -C "${repo}" clean -fdx
  git -C "${repo}" reset --hard HEAD
}
# }}}

# Verifying refs {{{
# Check if a ref name exists locally or in the remote repository.
# Usage:
#   verifyref REF [GIT_REPO_PATH]
verifyref() {
  local ref="${1}"
  # Defaults to git repository of cwd.
  local repo="${2:-$(git rev-parse --show-toplevel)}"

  if [ -z "${ref}" ]; then
    err "No ref provided"
    return 1
  fi

  # Check for both local and remote refs matching the given name.
  git -C "${repo}" rev-parse --verify "${ref}" >>/dev/null 2>&1 ||
    git -C "${repo}" ls-remote --exit-code "$(defaultremote "${repo}")" \
      "${ref}" >>/dev/null 2>&1
}
# }}}

# Git environment for shell {{{

# Prepare any extra Git-related shell functions for the current shell.
src_git_for_profile() {
  local workspace="${WS:-${HOME}/workspace}"

  # Source the forgit Git CLI if available.
  if [ -f "${workspace}/forgit/forgit.plugin.sh" ]; then
    . "${workspace}/forgit/forgit.plugin.sh"
  fi
}
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
