#!/usr/bin/env bash

# Include extra bash completions if available.
add_bash_completions() {
  local bashCompletionFile="/usr/share/bash-completion/bash_completion"

  # Source Bash completions in current shell if we've installed the completion
  # file.
  [ -f "${bashCompletionFile}" ] && . "${bashCompletionFile}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
