#!/bin/sh
# Contains custom Git functions that can be run in the terminal.

# General {{{

# Determines whether the given directory is in a Git repository.
# If no repository directory is provided, defaults to the current dir.
# Return 0 if the directory is a Git repo. Return non-zero otherwise.
isgitrepo() {
  local repoDir="${1:-$(git rev-parse --git-dir 2>/dev/null)}"

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

# Get a local representation of a remote branch name. If the input is already a
# local ref, just use that name. Thus,
#   origin/develop -> develop
#   master         -> master
localrefname() {
  local remoteRefName="${1}"
  echo "${remoteRefName#*/}"
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
  local createBranch=false
  # Character used to separate words in a branch name,
  # i.e., if this variable is set to ".", then an arg of
  # "this is my branch" will become "this.is.my.branch".
  local branchWordSeparator="${BRANCH_WORD_SEPARATOR:-.}"
  local OPTIND
  local o

  while getopts ":b:" o; do
    case "${o}" in
    b)
      # Create a new branch if '-b' provided.
      arg="${OPTARG}"
      createBranch=true
      ;;
      # We don't want a '*' default case because we want to be able to interpret
      # numeric arguments prefixed with '-' to traverse branch history.
    esac
  done
  shift $((OPTIND - 1))

  if [ -z "${arg}" ]; then
    arg="$(defaultbranch)"
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

  ref="$(echo -n "${ref}" |
    sed -e 's/^\s*//' -e 's/\s*$//' |
    tr '[ \t]' "${branchWordSeparator}")"
  if "${createBranch}" && ! verifyref "${ref}"; then
    git branch "${ref}" || {
      err "Could not create branch ${CYAN}${ref}${NC}"
      return 2
    }
  elif ! verifyref "${ref}"; then
    err "${CYAN}${ref}${NC} is not a valid Git ref"
    return 3
  fi

  if ! git checkout "${ref}"; then
    err "Could not switch to ref: ${CYAN}${ref}${NC}"
    return 4
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

# Get nth line from history file if arg is of the form '-<n>', where n is an
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
  local defaultRemote="$(defaultremote)"
  local currentBranch="$(currentref)"
  local branchListingCommand="git branch -a --format '%(refname:short)' |
    grep -E -v \
      -e '^(${defaultRemote}/)?${currentBranch}\$' \
      -e '^(${defaultRemote}/)?HEAD\$' |
    sort -u"

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  # Prioritized list of fuzzy search tools used to select the branch.
  if [ -x "$(command -v fzf)" ]; then
    branch="$(eval "${branchListingCommand}" | fzf)"
  elif [ -x "$(command -v fzy)" ]; then
    branch="$(eval "${branchListingCommand}" | fzy)"
  else
    branch="$(eval "${branchListingCommand}" | __default_list_branches)"
  fi

  # Strip off the leading "origin/" (or default remote) part of the branch name,
  # if necessary.
  branch="$(echo "${branch}" | sed "s|^\s*${defaultRemote}/||")"

  if [ -n "${branch}" ]; then
    sw "${branch}"
  fi
}

# Interactive switching program used when no fuzzy finder programs can be found.
__default_list_branches() {
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
  git branch -a --color=never --format="%(refname:short)" |
    grep -v '\WHEAD$' | sort -u
}

# Interactively delete branches. Safely removes important branches from the list
# of deletable branches, such as the default branch and the current branch.
dbi() {
  local defaultBranch="$(defaultbranch)"
  local currentBranch="$(currentref)"
  local branches
  local selectedBranches
  local branch
  local branchListingCommand="git branch --format='%(refname:short)' |
    grep -v -e '\WHEAD\$' -e '^${defaultBranch}\$' -e '^${currentBranch}\$' \
      -e '^master\$' -e '^main\$' -e '^develop\$' |
    tr ' ' '\n' |
    sort -u
  "

  branches=($(eval "${branchListingCommand}"))

  if [ -z "${branches[*]}" ]; then
    err "No applicable branches to delete"
    return 1
  fi

  # Prioritized list of fuzzy search tools used to select the branch.
  if [ -x "$(command -v fzf)" ]; then
    selectedBranches=($(echo "${branches[@]}" | tr ' ' '\n' | fzf))
  elif [ -x "$(command -v fzy)" ]; then
    selectedBranches=($(echo "${branches[@]}" | tr ' ' '\n' | fzy))
  else
    err "No fuzzy searching cli tool found"
    return 2
  fi

  if [ -z "${selectedBranches[*]}" ]; then
    return 0
  fi

  for branch in "${selectedBranches[@]}"; do
    log_info "Deleting branch: ${CYAN}${branch}${NC}"
    git branch -D "${branch}"
  done
}
# }}}

# Repository cleanup {{{

# Perform housekeeping on the given repository, mostly for compressing old refs,
# removing old remote tracking refs that no longer exist in the remote, etc.
gcleanup() {
  local repo="${1:-$(git rev-parse --show-toplevel)}"

  log_info "Cleaning repository: ${BLUE}${repo}${NC}"
  git -C "${repo}" gc --aggressive
  git -C "${repo}" fetch --prune --prune-tags
}
# }}}

# Committing {{{

# Shortcut for `git commit -m '<msg>'`. No need to put quote marks around your
# commit message. This function handles that bit for you.
gcm() {
  local message
  local attemptProjectCommit=${ATTEMPT_PROJECT_COMMIT:-true}
  local OPTIND
  local o

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  while getopts ":hm:p:" o; do
    case "${o}" in
    h)
      __gcm_help
      return 0
      ;;
    m)
      message="${OPTARG}"
      ;;
    p)
      attemptProjectCommit="${OPTARG}"
      ;;
    *)
      err "Unknown operand"
      __gcm_usage
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  if [ "$#" -gt 0 ]; then
    message="${*}"
    shift "$#"
  fi

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

  if "${attemptProjectCommit}" && validate_project_branch "$(currentref)"; then
    # Hook into the `pcm` project commit function if we're on a valid project
    # branch.
    pcm -m "${message}"
  else
    # Otherwise, make a standard commit.
    git commit -m "${message}"
  fi
}

__gcm_usage() {
  command cat <<EOF
USAGE:
  gcm [COMMIT_MSG]
      [-h]
      [-m COMMIT_MSG]
      [-p ATTEMPT_PROJECT_COMMIT]
EOF
}

__gcm_help() {
  command cat <<EOF
gcm - Alias function for 'git commit -m <msg>'

EOF
  __gcm_usage

  command cat <<EOF

OPTIONS:
  -h
    Print the help message and exit.

  -m COMMIT_MSG
    Supply the commit message as an optional argument. If this option is not
    provided, the commit message can be supplied as the primary argument to
    gcm. If the commit message is not supplied on the command line in any way,
    the user will be prompted for the message.

  -p ATTEMPT_PROJECT_COMMIT
    Boolean value determining whether gcm should attempt a project-style commmit
    if the current branch is formatted according to the style prescribed in the
    pcm function. The default behavior is to respect the ATTEMPT_PROJECT_COMMIT
    boolean environment variable, which itself defaults to true.
EOF
}
# }}}

# Cloning {{{

# Clone or update a Git repository, depending on whether the Git repository
# already exists on the file system.
clone_or_update_git_repo() {
  local repoUrl="${1}"
  local repoDestDir="${2}"
  local defaultBranch=""
  local currentRef=""

  if [ -z "${repoUrl}" ]; then
    err "No Git repository URL provided"
    return 1
  fi

  if [ -z "${repoDestDir}" ]; then
    repoDestDir="$(basename "${repoUrl%.git}")"
  fi

  if [ -d "${repoDestDir}" ]; then
    log_info "Updating Git repo: ${BLUE}${repoDestDir}${NC}"
    defaultBranch="$(defaultbranch "${repoDestDir}")"
    currentRef="$(git -C "${repoDestDir}" rev-parse --abbrev-ref HEAD)"
    git -C "${repoDestDir}" clean -df
    git -C "${repoDestDir}" restore --staged .
    git -C "${repoDestDir}" restore .
    if [ "${currentRef}" != "${defaultBranch}" ]; then
      # Switch to the default branch before pulling new objects.
      git -C "${repoDestDir}" checkout "${defaultBranch}"
    fi
    git -C "${repoDestDir}" pull
    if [ "${currentRef}" != "${defaultBranch}" ]; then
      # Switch back to the original ref if we switched to begin with.
      git -C "${repoDestDir}" checkout "${currentRef}"
    fi
  else
    log_info "Cloning Git repo at ${MAGENTA}${repoUrl}${NC} to" \
      "${BLUE}${repoDestDir}${NC}"
    git clone "${repoUrl}" "${repoDestDir}"
  fi
}
# }}}

# Diff {{{

# Show last diff either for a given file or for the entire last change set.
lastchange() {
  local file="$1"

  if [ -n "${file}" ] && [ ! -f "${file}" ] && [ ! -d "${file}" ]; then
    err "No such file or directory: ${BLUE}${file}${NC}"
    return 1
  fi

  if [ -n "${file}" ]; then
    git log -p -1 "${file}"
  else
    git log -p -1
  fi

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

# Listing files {{{

# List files in a directory by last git modification.
glsm() {
  local dirPath="${1:-$(pwd)}"

  git ls-tree -rz --name-only HEAD "${dirPath}" |
    TZ=UTC xargs -0 -I_ \
      git --no-pager log -1 --date=iso-local --format="%ad _" -- _ |
    sort
}

# List all files on the current branch that have changed from their version on
# master.
gchanged() {
  local currentBranch="$(currentref)"
  local defaultBranch="$(defaultbranch)"
  local mainBranch="${1}"

  if [ -z "${mainBranch}" ]; then
    mainBranch="${defaultBranch}"
  fi

  git diff --name-only "${currentBranch}" \
    "$(git merge-base "${currentBranch}" "${mainBranch}")"
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
  local gitPushCmd="git push -u"
  local OPTIND
  local o

  while getopts ":f" o; do
    case "${o}" in
    f)
      gitPushCmd="${gitPushCmd} -f"
      ;;
    *)
      err "Unknown operand: ${RED}${o}${NC}"
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  gitPushCmd="${gitPushCmd} '${defaultRemote}' '${currentRef}'"

  # Make sure we set the current branch to track the its remote counterpart.
  eval "${gitPushCmd}"
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
  local baseBranch="$(defaultbranch)"
  local commitMsg
  local currentBranch
  local response

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  currentBranch="$(currentref)"

  # Try to get base branch as positional param
  if [ -n "${1}" ]; then
    baseBranch="${1}"
    shift
  fi

  # Try to get commit message as positional param
  if [ -n "${*}" ]; then
    commitMsg="${*}"
  fi

  if [ "${currentBranch}" = "master" ] ||
    [ "${currentBranch}" = "develop" ]; then
    err "Not squashing commits on protected branch: ${currentBranch}"
    return 2
  fi

  log_debug "Using base branch: ${CYAN}${baseBranch}${NC}"
  log_debug "Branch to squash: ${CYAN}${currentBranch}${NC}"

  while [ -z "${commitMsg}" ]; do
    echoe "Please enter a commit message for the squashed commits:"
    read -r commitMsg
  done

  while ! echo "${response}" | grep -q "[YyNn]"; do
    command cat <<EOF
Do you wish to squash all commits on ${CYAN}${curentBranch}${NC} with message:
'${commitMsg}'? [y/n]
EOF
    read -r response
  done

  if echo "${response}" | grep -q "[Nn]"; then
    echoe "Aborting"
    return 3
  fi

  git reset "$(git merge-base "${baseBranch}" "${currentBranch}")"
  git add -A
  git commit -m "${commitMsg}"
}

# }}}

# Reverting/resetting {{{

# Reset changes to a Git repo, either soft or hard.
greset() {
  local numCommitsToReset
  local gitRepo
  local gitResetCmd
  local resetStyle="mixed"
  local OPTIND
  local o

  while getopts ":C:sfmMkn:" o; do
    case "${o}" in
    C)
      gitRepo="${OPTARG}"
      ;;
    s)
      resetStyle="soft"
      ;;
    f)
      resetStyle="hard"
      ;;
    m)
      resetStyle="mixed"
      ;;
    M)
      resetStyle="merge"
      ;;
    k)
      resetStyle="keep"
      ;;
    n)
      numCommitsToReset="${OPTARG}"
      ;;
    *)
      err "Unknown arg: ${o}"
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  # Try to get number of commits to reset as main arg.
  if [ -n "${1}" ]; then
    numCommitsToReset="${1}"
    shift
  fi

  if [ -z "${gitRepo}" ]; then
    gitRepo="$(git rev-parse --show-toplevel 2>/dev/null)"
  fi

  # Validate number of commits to reset.
  if ! echo "${numCommitsToReset}" | grep -q '^[1-9][0-9]*$'; then
    err "Invalid number of commits to reset: ${GREEN}${numCommitsToReset}${NC}"
    return 2
  fi

  gitResetCmd="git -C ${gitRepo} reset --${resetStyle}"
  gitResetCmd="${gitResetCmd} HEAD~${numCommitsToReset}"
  eval "${gitResetCmd}"
}
# }}}

# Removing files {{{

# git rm --cached
grmc() {
  local files="${@}"
  local gitRmCmd="git rm --cached"
  local f

  # Add recursive flags to file removal commands if we are working with
  # directories.
  for f in "${files[@]}"; do
    if [ -d "${f}" ]; then
      gitRmCmd="${gitRmCmd} -rf"
      break
    fi
  done

  gitRmCmd="${gitRmCmd} ${files[*]}"

  log_info "Removing files: ${BLUE}${files[*]}${NC} from Git repo"
  eval "${gitRmCmd}"
}
# }}}

# Verifying refs {{{

# Check if a ref (branch, tag, etc) exists locally.
verifylocalref() {
  local refName="${1}"

  git rev-parse --verify "${refName}" >>/dev/null 2>&1
}

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

# Stashing {{{

# Stash changes to local files, pull changes from remote, and pop stashed
# changes afterward.
gsp() {
  local filePaths="${*}"

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  if [ -z "${filePaths}" ]; then
    filePaths="$(git rev-parse --show-toplevel)"
  fi

  eval "git add ${filePaths}" &&
    git stash save &&
    git pull &&
    git stash pop
}
# }}}

# Hooks {{{

# Link the Git hooks scripts in the dotfiles repo to a given Git repository.
# If the user does not provide a Git repo, the current repository is assumed.
link_git_hooks() {
  local gitRepo="${1:-.}"
  local dotfilesGitHooksDir="${DOTFILES_COPY}/git_hooks"
  local targetHooksDir

  if ! isgitrepo "${gitRepo}"; then
    err "${BLUE}${gitRepo}${NC} is not contained within a Git repo"
    return 1
  fi

  targetHooksDir="$(git -C "${gitRepo}" rev-parse --git-dir 2>/dev/null)/hooks"

  if [ ! -d "${targetHooksDir}" ]; then
    mkdir -p "${targetHooksDir}"
  fi

  find "${dotfilesGitHooksDir}" -maxdepth 1 -mindepth 1 -type f -exec \
    ln -sf -t "${targetHooksDir}" '{}' \;
}
# }}}

# git-delta {{{
install_or_update_git_delta() {
  local gitDeltaUrl="https://github.com/dandavison/delta/releases/download/0.15.0/delta-0.15.0-x86_64-unknown-linux-gnu.tar.gz"
  local gitDeltaTarFilename="$(basename "${gitDeltaUrl}")"
  local gitDeltaDownloadDir="${HOME}"
  local gitDeltaDirname="${gitDeltaTarFilename/.tar.gz/}"
  local gitDeltaInstallPrefix="${HOME}/.local"

  (
    cd "${gitDeltaDownloadDir}" && \
    log_info "Downloading latest git-delta" && \
    curl -sL -o "${gitDeltaTarFilename}" "${gitDeltaUrl}" && \
    log_info "Extracting git-delta archive" && \
    tar -zxvf "${gitDeltaTarFilename}" && \
    log_info "Moving delta to ${gitDeltaInstallPrefix}/bin/" && \
    mv "${gitDeltaDirname}/delta" "${gitDeltaInstallPrefix}/bin/delta" && \
    log_info "Removing downloaded artifacts" && \
    rm -rf "${gitDeltaDirname:?}" && \
    rm -f "${gitDeltaTarFilename:?}"
  )
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

# URL {{{
# Turn the default HTTPS Git remote URL into an SSH URL.
# This function's inverse is gitsshtohttp.
githttptossh() {
  local originFetch=""
  local originFetchSsh=""
  local defaultRemote=""

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  defaultRemote="$(defaultremote)"
  originFetch="$(git remote -v |
    grep -e "${defaultRemote}" -e 'fetch' |
    awk '{print $2}' |
    tr -d '\n')"

  if echo "${originFetch}" | grep -q '^https'; then
    originFetchSsh="$(echo "${originFetch}" |
      sed -E -e 's|https://([^.]+\.[a-z]{3})/|git@\1:|' -e 's/(\.git).*$/\1/' |
      tr -d '\n')"
    log_info "Setting fetch/pull URL for ${MAGENTA}${defaultRemote}${NC} to:" \
      "${CYAN}${originFetchSsh}${NC}"
    git remote set-url origin "${originFetchSsh}"
  fi
}

# Turn the default SSH Git remote URL into an HTTPS URL.
# This function's inverse is githttptossh.
gitsshtohttp() {
  local originFetch=""
  local originFetchHttp=""
  local defaultRemote=""

  if ! isgitrepo; then
    err "Not in a Git repository"
    return 1
  fi

  defaultRemote="$(defaultremote)"
  originFetch="$(git remote -v |
    grep -e "${defaultRemote}" -e 'fetch' |
    awk '{print $2}' |
    tr -d '\n')"

  if echo "${originFetch}" | grep -q '^git@'; then
    originFetchHttp="$(echo "${originFetch}" |
      sed -E -e 's|git@([^.]+\.[a-z]{3}):|https://\1/|' -e 's/(\.git).*$/\1/' |
      tr -d '\n')"
    log_info "Setting fetch/pull URL for ${MAGENTA}${defaultRemote}${NC} to:" \
      "${CYAN}${originFetchHttp}${NC}"
    git remote set-url origin "${originFetchHttp}"
  fi
}
# }}}

# Lazygit {{{

# Install the lazygit commandline Git porcelain for an Ubuntu distribution.
install_lazygit() {
  local lazygitVersion="$(curl -sL \
    "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" |
    grep -Po '"tag_name":\s*"v\K[^"]*'
  )"
  (
    log_info "Downloading latest lazygit version" && \
    curl -sL -o lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${lazygitVersion}_Linux_x86_64.tar.gz" && \
    tar -zxvf lazygit.tar.gz lazygit && \
    sudo install lazygit /usr/local/bin && \
    rm -f 'lazygit.tar.gz' 'lazygit'
  )
}

# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
