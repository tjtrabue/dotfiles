#!/bin/sh

# If the `bat` tool is installed, add the following functions:
if [ -x "$(command -v bat)" ]; then
  # Combine `bat` with git-diff to view lines around code changes with proper
  # syntax highlighting:
  bdiff() {
    git diff --name-only --diff-filter=d | xargs bat --diff
  }

  # Combine `bat` with git-show to display an older version of a file with
  # syntax highlighting.
  bshow() {
    git show "${@}" | bat -l rs
  }

  # Tail a given file using bat's syntax highlighting capabilities
  btf() {
    local logFile="${*}"

    tail -f "${logFile}" | bat --paging=never -l log
  }
fi

# vim:foldenable:foldmethod=indent:foldlevel=1:foldnestmax=1
