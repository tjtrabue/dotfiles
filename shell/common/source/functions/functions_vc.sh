#!/bin/sh

# These functions work for any version control system (VCS).

# Wrapper function that determines if the given directory (or current directory
# if no directory argument is given) belongs to a VCS.
isrepo() {
  local repo="${1:-$(pwd)}"
  isgitrepo "${repo}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
