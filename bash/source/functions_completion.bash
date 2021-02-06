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
    __add_git_command_alias_completions "add" "ga" "_git_add"
    __add_git_command_alias_completions "branch" "gb" "_git_branch"
    __add_git_command_alias_completions "commit" "gc" "_git_commit"
    __add_git_command_alias_completions "clone" "gcl" "_git_clone"
    __add_git_command_alias_completions "clean" "gcle" "_git_clean"
    __add_git_command_alias_completions "checkout" "gco" "_git_checkout"
    __add_git_command_alias_completions "config" "gcon" "_git_config"
    __add_git_command_alias_completions "diff" "gd" "_git_diff"
    __add_git_command_alias_completions "describe" "gdes" "_git_describe"
    __add_git_command_alias_completions "merge" "gm" "_git_merge"
    __add_git_command_alias_completions "push" "gp" "_git_push"
    __add_git_command_alias_completions "pull" "gpl" "_git_pull"
    __add_git_command_alias_completions "rebase" "grb" "_git_rebase"
    __add_git_command_alias_completions "remote" "gre" "_git_remote"
    __add_git_command_alias_completions "reset" "gres" "_git_reset"
    __add_git_command_alias_completions "restore" "grest" "_git_restore"
    __add_git_command_alias_completions "revert" "grev" "_git_revert"
    __add_git_command_alias_completions "status" "gs" "_git_status"
    __add_git_command_alias_completions "show-branch" "gsb" "_git_show_branch"
    __add_git_command_alias_completions "stash" "gst" "_git_stash"
    __add_git_command_alias_completions "switch" "gsw" "_git_switch"
    __add_git_command_alias_completions "tag" "gt" "_git_tag"
  fi
}

# Mark a particular alias a git command for the purpose of adding shell
# completions, such as using `gco` for `git checkout` completions.
__add_git_command_alias_completions() {
  local cmd="${1}"
  local cmdAlias="${2}"
  local completionFunc="${3}"

  eval "alias ${cmdAlias}='git ${cmd}'"
  eval "__git_complete ${cmdAlias} ${completionFunc}"
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
