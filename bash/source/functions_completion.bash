#!/usr/bin/env bash

# Include extra bash completions if available.
add_bash_completions() {
  local bashCompletionHome="/usr/share/bash-completion"
  local bashCompletionFile="${bashCompletionHome}/bash_completion"

  # Source Bash completions in current shell if we've installed the completion
  # file.
  [ -f "${bashCompletionFile}" ] && . "${bashCompletionFile}"

  # Add extra completions to special command aliases, such as `g` for `git`,
  # and `r` for `docker`.
  __add_git_alias_completions
  __add_docker_alias_completions
}

# Add lots of cool completions for git aliases, such as `g` for `git`, and
# `gco` for `git checkout`.
__add_git_alias_completions() {
  # Recognize `g` as an alias for `git`, thereby enabling git command line
  # completions for `g`.
  __add_completions_to_command_alias "git" "g"

  # Add more specific git alias completions
  if funcp "__git_complete"; then
    alias ga='git add' && __git_complete ga _git_add
    alias gb='git branch' && __git_complete gb _git_branch
    alias gc='git commit' && __git_complete gc _git_commit
    alias gcl='git clone' && __git_complete gcl _git_clone
    alias gcle='git clean' && __git_complete gcle _git_clean
    alias gco='git checkout' && __git_complete gco _git_checkout
    alias gcon='git config' && __git_complete gcon _git_config
    alias gd='git diff' && __git_complete gd _git_diff
    alias gdes='git describe' && __git_complete gdes _git_describe
    alias gm='git merge' && __git_complete gm _git_merge
    alias gp='git push' && __git_complete gp _git_push
    alias gpl='git pull' && __git_complete gpl _git_pull
    alias grb='git rebase' && __git_complete grb _git_rebase
    alias gre='git remote' && __git_complete gre _git_remote
    alias gres='git reset' && __git_complete gres _git_reset
    alias grest='git restore' && __git_complete grest _git_restore
    alias grev='git revert' && __git_complete grev _git_revert
    alias gs='git status' && __git_complete gs _git_status
    alias gsb='git show-branch' && __git_complete gsb _git_show_branch
    alias gst='git stash' && __git_complete gst _git_stash
    alias gsw='git switch' && __git_complete gsw _git_switch
    alias gt='git tag' && __git_complete gt _git_tag
  fi
}

# Add lots of cool docker completions to aliased docker commands.
__add_docker_alias_completions() {
  # Recognize `r` as an alias for `docker`, thereby enabling docker command line
  # completions for `r`.
  # NOTE: The `d` alias is already taken by `fasd` command line tool suite.
  __add_completions_to_command_alias "docker" "r"
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

  # Actually create the alias before we add completions.
  eval "alias ${cmdAlias}='${cmd}'"

  if funcp "_completion_loader"; then
    # Use the dynamic completion loader function included with bash-completion
    # if available.
    _completion_loader "${cmd}"
  elif [ -f "${extraCompletionsDir}/${cmd}" ]; then
    # Otherwise, try and source the command's completion file the old fashioned
    # way.
    . "${extraCompletionsDir}/${cmd}"
  fi

  completionCmdArr=($(complete -p "${cmd}"))
  # `complete -p <cmd>` returns the command needed to enable bash completions
  # for <cmd>. Thus, what we want to do is eval that command, but to do so
  # replacing the final term (the command) with an alias.
  eval "${completionCmdArr[*]::${#completionCmdArr[@]}-1} ${cmdAlias}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
