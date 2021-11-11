#!/bin/sh
# Contains custom Git functions that can be run in the terminal.

# General {{{

# Determines whether the given directory is in a Git repository.
# If no repository directory is provided, defaults to the current dir.
# Return 0 if the directory is a Git repo. Return non-zero otherwise.
isgitrepo() {
  local repoDir="${1:-$(git rev-parse --show-toplevel)}"

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
  local gitRepo="${1:-$(git rev-parse --show-toplevel)}"

  if ! isgitrepo "${gitRepo}"; then
    err "${BLUE}${gitRepo}${NC} is not a Git repository."
    return 1
  fi

  git -C "${gitRepo}" remote 2>/dev/null
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
  local histFile="${SW_HISTORY_FILE:-${HOME}/.sw_ref_hist}"
  # We want to write the current branch/commit to history after we switch.
  local currentRef="$(currentref)"

  if [ -z "${arg}" ]; then
    err "No ref provided to switch"
    return 1
  fi

  if echo "${arg}" | grep -q '^-[0-9]$'; then
    # Get nth line from history file if arg is of the form'-<n>' where n is an
    # integer.
    ref="$(sed "${arg#-}q;d" "${histFile}")"
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
    __save_ref_to_sw_hist "${currentRef}"
  fi
}

# Intelligently determine whether a local copy of a remote branch exists. If it
# does not, we must first create a local copy and instruct the branch to track
# its remote counterpart. If the local copy already exists, we have only to
# switch to it.
__checkout_local_or_remote_branch() {
  local remoteBranch="${1}"
  local localBranch="$(basename "${remoteBranch}" 2>/dev/null)"
  local currentRef="$(currentref)"

  if [ -z "${remoteBranch}" ]; then
    err "No remote branch name provided"
    return 1
  elif [ "${localBranch}" = "${currentRef}" ]; then
    warn "HEAD is already set to ref: ${CYAN}${localBranch}${NC}"
    return 0
  fi

  if ! verifyref "${localBranch}"; then
    git checkout -t "${remoteBranch}"
  else
    git checkout "${localBranch}"
  fi
}

# Persist most recent ref passed to `sw' to the branch history file.
__save_ref_to_sw_hist() {
  local ref="${1}"
  local histFile="${SW_HISTORY_FILE:-${HOME}/.sw_ref_hist}"
  local histTempFile="${histFile}.tmp"
  local numRefsToSave="10"

  if [ -z "${ref}" ]; then
    err "No ref provided to save to sw history file"
    return 1
  fi

  if [ -f "${histFile}" ]; then
    log_debug "Writing sw ref to history file: ${GREEN}${histFile}${NC}"

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

# Interactive branch switching using fuzzy search program.
swi() {
  local branch
  local branchListingCommand="git branch -a |
    grep -v -e '\*' -e 'HEAD' |
    sed -e 's/\s*->.*//' -e 's/^\s*//'"

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
# }}}

# Submodules {{{
# Lists all submodules in a repo
ls-submods() {
  local repoHome

  if git rev-parse --is-inside-work-tree >>/dev/null; then
    repoHome="$(dirname "$(git rev-parse --git-dir)")"
    grep path "$repoHome/.gitmodules" | sed 's/.*= //'
  else
    err "Not in a git repository"
    return 1
  fi
}

# Creates a git submodule based on a git url.
# Alternately deletes a submodule in a repo:
submod() {
  if [ "$1" = "-r" ]; then
    if [ -z "$2" ]; then
      err "Must enter the path to the submodule"
      return 1
    fi

    local submods=($(ls_submods))
    for sub in "${submods[@]}"; do
      if [ "$2" = "$sub" ]; then
        echoe "Removing submodule $2"
        mv "$2" "$2_tmp"
        git submodule deinit "$2"
        git rm --cached "$2"
        mv "$2_tmp" "$2"
        rm -rf "$(git rev-parse --git-dir)/modules/$2"
      fi
    done
  else
    git submodule add "$1"
  fi
}

# Update all submodules in the current git repo (requires git version
# 1.6.1 or later)
upsubs() {
  if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git submodule foreach git pull origin master
  else
    err "Not in a git repository"
    return 1
  fi
}
# }}}

# Reverting/resetting {{{
# Reverts the current repo to the state of its previous commit:
creset() {
  if git rev-parse --is-inside-work-tree >>/dev/null 2>&1; then
    git reset --soft HEAD~1
    git reset HEAD "$(git rev-parse --show-toplevel)"
  else
    return 1
  fi
}

# Resets all uncommitted changes in the current repository
ucreset() {
  if git rev-parse --is-inside-work-tree >>/dev/null 2>&1; then
    # Revert changes to modified files.
    git reset --hard
    # Remove all untracked files and directories.
    # (`-d` is `remove directories`, `-ff` is `force`)
    git clean -d -ff
  else
    return 1
  fi
}

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
    cat <<EOF
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

  git push origin HEAD
}
# }}}

# Git environment for shell {{{

# Prepare any extra Git-related shell functions for the current shell.
src_git_for_profile() {
  # Source the forgit Git CLI if available.
  if [ -f "${WS}/forgit/forgit.plugin.sh" ]; then
    . "${WS}/forgit/forgit.plugin.sh"
  fi
}
# }}}

# Verifying refs {{{
verifyref() {
  local ref="${1}"

  if [ -z "${ref}" ]; then
    err "No ref provided"
    return 1
  fi

  git rev-parse --verify "${ref}" >>/dev/null 2>&1
}
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
