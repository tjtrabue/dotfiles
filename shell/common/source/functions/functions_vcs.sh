#!/bin/sh

# These functions work for any version control system (VCS).

# Wrapper function that determines if the given directory (or current directory
# if no directory argument is given) belongs to a VCS.
isrepo() {
  local repo="${1:-$(pwd)}"
  isgitrepo "${repo}"
}

# Get the path to the root of the repository that the given directory (or the
# current directory) belongs to.
getreporoot() {
  local repo="${1:-$(pwd)}"

  if ! isrepo "${repo}"; then
    err "Directory ${repo} is not part of a VCS repository"
    return 1
  fi

  if isgitrepo "${repo}"; then
    log_info "Getting root of Git repository for directory: ${repo}"
    git -C "${repo}" rev-parse --show-toplevel
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
