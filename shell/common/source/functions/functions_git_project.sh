#!/bin/sh

# Contains functions specifically related to interacting with Git repositories
# as software projects.

#==========================================#
#========== Create Project Branch =========#
#==========================================#

# Pabst Blue Ribbon...
# Nah, it's "project branch".
#
# Usage:
#   pbr TASK_NUMBER [DESC] [PROJECT_ID]
pbr() {
  local taskNumber
  local description
  local projectIdentifier
  local projectBranchName
  local defaultRemote="$(defaultremote)"
  local OPTIND
  local o

  while getopts ":hn:d:p:" o; do
    case "${o}" in
    h)
      __pbr_help
      return 0
      ;;
    n)
      taskNumber="${OPTARG}"
      ;;
    d)
      description="${OPTARG}"
      ;;
    p)
      projectIdentifier="${OPTARG}"
      ;;
    *)
      err "Unknown operand"
      __pbr_usage
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  if [ -z "${taskNumber}" ] && [ -n "${1}" ] &&
    echo "${1}" | grep -E -q '^[0-9]+$'; then
    taskNumber="${1}"
    log_debug "Got task number from positional parameter:" \
      "${YELLOW}${taskNumber}${NC}"
    shift
  fi

  if [ -z "${description}" ] && [ "$#" -gt 0 ]; then
    description="${*}"
    log_debug "Got description from command line:" \
      "'${YELLOW}${description}${NC}'"
    shift "$#"
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

  if [ "$(currentref)" = "${projectBranchName}" ]; then
    warn "Already on branch ${CYAN}${projectBranchName}${NC}"
  elif verifylocalref "${projectBranchName}" ||
    verifylocalref "${defaultRemote}/${projectBranchName}"; then
    log_info "Branch ${CYAN}${projectBranchName}${NC} already exists;" \
      "switching to it"
    # Want to use `sw` to make use of its history tracking capabilties.
    sw "${projectBranchName}"
  else
    log_info "Creating new project branch: ${CYAN}${projectBranchName}${NC}"
    git branch "${projectBranchName}"
    # Want to use `sw` to make use of its history tracking capabilties.
    sw "${projectBranchName}"
  fi
}

__pbr_usage() {
  command cat <<EOF
pbr - Create and switch to Git branches for current software project

USAGE:
  pbr [TASK_NUMBER] [DESCRIPTION]
      [-h]
      [-d DESCRIPTION]
      [-n TASK_NUMBER]
      [-p PROJECT_IDENTIFIER]
EOF
}

__pbr_help() {
  __pbr_usage
  command cat <<EOF

OPTIONS:
  -h
    Print the help message (this message) and exit.

  -d DESCRIPTION
    Supply the description for the project branch as an optional argument.

  -n TASK_NUMBER
    Supply the task number for the project branch as an optional argument.

  -p PROJECT_IDENTIFIER
    Supply the project ID for the project branch as an optional argument.

ENVIRONMENT VARIABLES:
  Each of these environment variables may be placed in a per-project file named
  '${PROJECT_ENV_FILE_NAME}' located in the project's root directory.

  PROJECT_IDENTIFIER
    The project ID string that prefixes each project branch. It is a string of
    upper-case letters, such as 'PROJ'.

  PROJECT_FIELD_SEPARATOR
    The character separating primary fields in a project-task item. If this
    environment variable is not set, default value is '-'.

    Example from branch name: PROJ-1234.add.cool.feature
    Example from commit message: PROJ-1234: Added a cool feature

    The field separator here is '-' since it delineates the primary aspects of
    the project task under development.

  PROJECT_WORD_SEPARATOR
    The character separating word fields in the description part of the project
    branch name. If this environment variable is not set, the default value is
    '.'.

    Example from branch name: PROJ-1234.add.cool.feature

    The word separator here is '.' since it separates the words in the
    description.
EOF
}

__construct_project_branch() {
  local taskNumber="${1}"
  local description="${2}"
  local projectIdentifier="${3}"
  local projectFieldSep="${PROJECT_FIELD_SEPARATOR:--}"
  local projectWordSep="${PROJECT_WORD_SEPARATOR:-.}"
  local formatString="%s${projectFieldSep}%s${projectWordSep}%s"
  local formattedDescription="$(echo "${description}" |
    sed -E "s/\s+/${projectWordSep}/g")"

  printf "${formatString}" \
    "${projectIdentifier}" \
    "${taskNumber}" \
    "${formattedDescription}"
}

#===================================#
#========== Project Commit =========#
#===================================#

# Commit for project.
pcm() {
  local taskNumber
  local commitMsg
  local projectIdentifier
  local commitMsgFormat="${PROJECT_MSG_STYLE}"
  local finalCommitMsg
  local OPTIND
  local o

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

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  # Try to get task number as first positional parameter if the first positional
  # parameter is a number.
  if [ -z "${taskNumber}" ] && [ -n "${1}" ] &&
    echo "${1}" | grep -E -q '^[0-9]+$'; then
    taskNumber="${1}"
    log_debug "Got task number as positional parameter:" \
      "${YELLOW}${taskNumber}${NC}"
    shift
  fi
  # If we did not get the task number as a positional parameter, try to parse
  # it from the current branch name.
  if [ -z "${taskNumber}" ]; then
    log_debug "Trying to get task number from branch name..."
    taskNumber="$(__parse_branch_for_task_number "$(currentref)")"
    log_debug "Task number is now: ${YELLOW}${taskNumber}${NC}"
  fi

  if [ -z "${commitMsg}" ] && [ "$#" -gt 0 ]; then
    log_debug "Getting commit message from command line"
    commitMsg="${*}"
    shift "$#"
  fi

  # If we did not get the project ID another way, try to parse it from the
  # current branch name.
  if [ -z "${projectIdentifier}" ]; then
    log_debug "Trying to get project ID from branch name..."
    projectIdentifier="$(__parse_branch_for_project_id "$(currentref)")"
    log_debug "Project ID is now: ${YELLOW}${projectIdentifier}${NC}"
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

  log_debug "Final commit message: '${GREEN}${finalCommitMsg}${NC}'"
  git commit -m "${finalCommitMsg}"
}

__pcm_usage() {
  command cat <<EOF
USAGE:
  pcm [TASK_NUMBER] [COMMIT_MSG]
      [-h]
      [-m COMMIT_MSG]
      [-n TASK_NUMBER]
      [-p PROJECT_IDENTIFIER]
      [-f MSG_FORMAT]
EOF
}

__pcm_help() {
  command cat <<EOF
pcm - Create Git commits for current software project

EOF
  __pcm_usage
  command cat <<EOF

OPTIONS:
  -h
    Print the help message (this message) and exit.

  -m COMMIT_MSG
    Supply the commit message as an optional argument. If this option is
    omitted, the user may supply the commit message as a positional parameter.
    Otherwise, the user will be prompted to enter the it interactively.

  -n TASK_NUMBER
    Supply the task number. If this option is omitted, the user may supply the
    task number as a positional parameter. The branch name will also be parsed
    for a possible task number. In all other cases, the user will be prompted
    to enter the the task number interactively.

  -p PROJECT_IDENTIFIER
    Supply the project ID string. If no other value for this parameter is
    supplied, its value will be read from the PROJECT_IDENTIFIER environment
    variable, then the branch name will be parsed and, failing those, the user
    will be prompted for the value of the project ID interactively.

  -f MSG_FORMAT
    The format for the commit message. The value of this variable can be any
    value that is valid for PROJECT_MSG_STYLE. See the section on
    PROJECT_MSG_STYLE below for more details.

ENVIRONMENT VARIABLES:
  Each of these environment variables may be placed in a per-project file named
  '${PROJECT_ENV_FILE_NAME}' located in the project's root directory.

  PROJECT_IDENTIFIER
    The project ID string to use for the current project. It's value should look
    like 'PROJ'.

  PROJECT_MSG_STYLE
    Should be one of the strings listed in the examples section below. This
    variable determines how the commit message string will be formatted. If
    this environment variable is not set, 'colon' will be used by default.
                     Examples:
                       colon -> "PROJ-1234: This is the commit message"
                       braces -> "[PROJ-1234] This is the commit message"
                       nopunct -> "PROJ-1234 This is the commit message"

  PROJECT_FIELD_SEPARATOR
    The character separating primary fields in a project-task item. If this
    environment variable is not set, default value is '-'.

    Example from branch name: PROJ-1234.add.cool.feature
    Example from commit message: PROJ-1234: Added a cool feature

    The field separator here is '-' since it delineates the primary aspects of
    the project task under development.
EOF
}

# Return 0 if input branch name is of the form 'PROJ-1234...'.
# Return an error code, otherwise.
validate_project_branch() {
  local branchName="${1}"
  local projectFieldSep="${PROJECT_FIELD_SEPARATOR:--}"

  if [ -z "${branchName}" ]; then
    branchName="$(currentref)"
  fi

  echo "${branchName}" |
    grep -E -q -o "[A-Z]+${projectFieldSep}[0-9]+.*"
}

# Try to get the project ID string from the current branch name.
# For example:
#   PROJ-1234 -> PROJ
__parse_branch_for_project_id() {
  local branchName="${1}"

  if validate_project_branch "${branchName}"; then
    echo "${branchName}" | grep -E --color=never -o '^[A-Z]+'
  fi
}

# Try to get the task number from the current branch name.
# For example:
#   PROJ-1234 -> 1234
__parse_branch_for_task_number() {
  local branchName="${1}"

  if validate_project_branch "${branchName}"; then
    echo "${branchName}" | grep -E --color=never -o '[0-9]+'
  fi
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
  local projectFieldSep="${PROJECT_FIELD_SEPARATOR:--}"

  if [ -z "${commitMsgFormat}" ]; then
    commitMsgFormat="colon"
  fi

  case "${commitMsgFormat}" in
  'braces')
    formatString="[%s${projectFieldSep}s] %s"
    ;;
  'nopunct')
    formatString="%s${projectFieldSep}%s %s"
    ;;
  *)
    formatString="%s${projectFieldSep}%s: %s"
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
  local projectFieldSep="${PROJECT_FIELD_SEPARATOR:--}"

  if ! echo "${commitMsg}" | grep -E -q \
    -e "^[A-Z]+${projectFieldSep}[0-9]+:\s+.*\$" \
    -e "^[A-Z]+${projectFieldSep}[0-9]+\s+.*\$" \
    -e "^\[[A-Z]+${projectFieldSep}[0-9]+\]\s+.*\$"; then
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
