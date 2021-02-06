#!/usr/bin/env bash

# Include extra bash completions if available.
add_bash_completions() {
  local bashCompletionHome="/usr/share/bash-completion"
  local bashCompletionFile="${bashCompletionHome}/bash_completion"

  # Source Bash completions in current shell if we've installed the completion
  # file.
  [ -f "${bashCompletionFile}" ] && . "${bashCompletionFile}"

  # Recognize `r` as an alias for `docker`, thereby enabling docker command line
  # completions for `r`.
  # NOTE: The `d` alias is taken by `fasd` command line tool suite.
  __add_completions_to_command_alias "docker" "r"

  # Same goes for `g` and `git`.
  __add_completions_to_command_alias "git" "g"
}

# Enable shell completions for a command alias, such as `g` for `git`.
__add_completions_to_command_alias() {
  local cmd="${1}"
  local cmdAlias="${2}"
  local bashCompletionHome="/usr/share/bash-completion"
  local extraCompletionsDir="${bashCompletionHome}/completions"
  local completionCmdArr=()

  if [ -z "${cmd}" ]; then
    err "No command provided"
    return 1
  elif [ -z "${cmdAlias}" ]; then
    err "No command alias provided"
    return 2
  fi

  # `complete -p <cmd>` returns the command needed to enable bash completions
  # for <cmd>. Thus, what we want to do is eval that command, but to do so
  # replacing the final term (the command) with an alias.
  if [ -f "${extraCompletionsDir}/${cmd}" ]; then
    . "${extraCompletionsDir}/${cmd}"
    completionCmdArr=($(complete -p "${cmd}"))
    # Expand the array representing the `complete` command, but do not include
    # the final element. Instead, replace it with the command's alias.
    eval "${completionCmdArr[*]::${#completionCmdArr[@]}-1} ${cmdAlias}"
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
