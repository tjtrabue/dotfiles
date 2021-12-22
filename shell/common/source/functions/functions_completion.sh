#!/bin/sh

# Include extra shell completions for whatever shell the user currently uses.
add_shell_completions() {
  local currentShell="$(currentshell)"

  if [ "${currentShell}" = "bash" ]; then
    add_bash_completions
  elif [ "${currentShell}" = "zsh" ]; then
    add_zsh_completions
  elif [ "${currentShell}" = "fish" ]; then
    add_fish_completions
  fi
}

# Include extra bash completions if available.
add_bash_completions() {
  local bashCompletionHome="/usr/share/bash-completion"
  local bashCompletionFile="${bashCompletionHome}/bash_completion"

  # Source Bash completions in current shell if we've installed the completion
  # file.
  [ -f "${bashCompletionFile}" ] && . "${bashCompletionFile}"

  # Source additional manually installed completion files.
  __source_extra_bash_user_completions

  # Add extra completions to special command aliases
  __add_git_alias_completions
  __add_custom_zsh_git_completions
  __add_docker_alias_completions
  __add_sdkman_completions
  __add_asdf_completions_for_bash
}

# Add any extra Zsh completions.
# NOTE ABOUT ZSH: Install and activate Zsh completions with a plugin manager,
#                 such as zplug.
add_zsh_completions() {
  __init_zsh_completions
  __add_zsh_git_completions
  __add_zsh_docker_completions
  __add_extra_zsh_completions
  __add_asdf_completions_for_zsh
  __init_zsh_completions
}

# Add extra Fish shell completions.
add_fish_completions() {
  __add_asdf_completions_for_fish
}

# Initialize the Zsh completion system manually. This is sometimes necessary, as
# Zsh does not automatically activate its completions at first.
__init_zsh_completions() {
  autoload -Uz compinit && compinit
}

__get_zsh_completion_dir() {
  printf "${ZDOTDIR:-${HOME}/.zsh}/completion"
}

__create_zsh_completion_dir() {
  local completionDir="$(__get_zsh_completion_dir)"

  if [ ! -d "${completionDir}" ]; then
    log_info "Creating ZSH completion dir: ${BLUE}${completionDir}${NC}"
    mkdir -p "${completionDir}"
  fi
}

__add_zsh_completion_dir_to_fpath() {
  local completionDir="$(__get_zsh_completion_dir)"

  # Add completion files to function path
  fpath=("${completionDir}" $fpath)
}

# Download extra Zsh completions for git.
__add_zsh_git_completions() {
  # Zsh provides Git completions by default, so this is no longer necesary.

  local completionDir="$(__get_zsh_completion_dir)"
  local bashCompletionFile="${completionDir}/git-completion.bash"
  local zshGitCompletionFile="${completionDir}/_git"

  __create_zsh_completion_dir

  if [ ! -f "${bashCompletionFile}" ]; then
    curl -o "${bashCompletionFile}" \
      "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash"
  fi
  if [ ! -f "${zshGitCompletionFile}" ]; then
    curl -o "${zshGitCompletionFile}" \
      "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh"
  fi

  __add_zsh_completion_dir_to_fpath
  __add_custom_zsh_git_completions
}

# Download Zsh docker completions.
__add_zsh_docker_completions() {
  local completionDir="$(__get_zsh_completion_dir)"
  local zshDockerCompletionFile="${completionDir}/_docker"

  __create_zsh_completion_dir

  if [ ! -f "${zshDockerCompletionFile}" ]; then
    curl -o "${zshDockerCompletionFile}" \
      "https://raw.githubusercontent.com/docker/cli/master/contrib/completion/zsh/_docker"
  fi

  __add_zsh_completion_dir_to_fpath
}

# Add Zsh completions for custom Git functions from this repository.
__add_custom_zsh_git_completions() {
  # We want `sw` to autocomplete Git refs, such as branches and tags.
  compdef _git sw=git-branch
}

# Install additional Zsh command line completions
__add_extra_zsh_completions() {
  local repoUrl="git://github.com/zsh-users/zsh-completions.git"
  local repoDir="${HOME}/.zsh_completions"

  log_info "Adding extra Zsh completions"

  if [ ! -d "${repoDir}" ]; then
    log_info "Cloning zsh-completions repository to: ${BLUE}${repoDir}${NC}"
    git clone "${repoUrl}" "${repoDir}"
  fi

  fpath=("${repoDir}/src" $fpath)
}

# Source bash completion files installed manually by the user.
__source_extra_bash_user_completions() {
  local f
  local completionDir="${USER_BASH_COMPLETION_DIR:-${HOME}/.bash_completion.d}"

  if [ -d "${completionDir}" ]; then
    for f in $(find "${completionDir}" -type f); do
      . "${f}"
    done
  fi
}

# Add lots of cool completions for git aliases, such as `g` for `git`, and
# `gco` for `git checkout`.
__add_git_alias_completions() {
  # Recognize `g` as an alias for `git`, thereby enabling git command line
  # completions for `g`.
  __add_completions_to_command_alias "git" "g"

  # Add more specific git alias completions
  if isfunc "__git_complete"; then
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
    __add_git_command_alias_completions "rm" "grm" "_git_rm"
    __add_git_command_alias_completions "status" "gs" "_git_status"
    __add_git_command_alias_completions "show-branch" "gsb" "_git_show_branch"
    __add_git_command_alias_completions "stash" "gst" "_git_stash"
    __add_git_command_alias_completions "switch" "gsw" "_git_switch"
    __add_git_command_alias_completions "tag" "gt" "_git_tag"
  fi
}

# Mark a particular alias as a git command for the purpose of adding shell
# completions, such as using `gco` for `git checkout` completions.
__add_git_command_alias_completions() {
  local cmd="${1}"
  local cmdAlias="${2}"
  local completionFunc="${3}"

  eval "alias ${cmdAlias}='git ${cmd}'"
  eval "__git_complete ${cmdAlias} ${completionFunc}"
}

# Here we add Bash completions for custom Git functions that we have written.
__add_custom_bash_git_completions() {
  if isfunc "__git_complete"; then
    __git_complete sw _git_branch
  fi
}

# Add lots of cool docker completions to aliased docker commands.
__add_docker_alias_completions() {
  # Recognize `d` as an alias for `docker`, thereby enabling docker command line
  # completions for `d`.
  __add_completions_to_command_alias "docker" "d"

  # Add extra docker aliases and completions.
  __add_docker_command_alias_completions "attach" "dat" "_docker_attach"
  __add_docker_command_alias_completions "build" "dbu" "_docker_build"
  __add_docker_command_alias_completions "cp" "dcp" "_docker_cp"
  __add_docker_command_alias_completions "diff" "ddi" "_docker_diff"
  __add_docker_command_alias_completions "help" "dh" "_docker_help"
  __add_docker_command_alias_completions "image" "dim" "_docker_image"
  __add_docker_command_alias_completions "images" "dims" "_docker_images"
  __add_docker_command_alias_completions "info" "din" "_docker_info"
  __add_docker_command_alias_completions "inspect" "dins" "_docker_inspect"
  __add_docker_command_alias_completions "kill" "dk" "_docker_kill"
  __add_docker_command_alias_completions "login" "dlogin" "_docker_login"
  __add_docker_command_alias_completions "logout" "dlogout" "_docker_logout"
  __add_docker_command_alias_completions "logs" "dlo" "_docker_logs"
  __add_docker_command_alias_completions "pause" "dpa" "_docker_pause"
  __add_docker_command_alias_completions "ps" "dps" "_docker_ps"
  __add_docker_command_alias_completions "pull" "dpl" "_docker_pull"
  __add_docker_command_alias_completions "restart" "dre" "_docker_restart"
  __add_docker_command_alias_completions "rm" "drm" "_docker_rm"
  __add_docker_command_alias_completions "run" "drn" "_docker_run"
  __add_docker_command_alias_completions "stop" "dst" "_docker_stop"
  __add_docker_command_alias_completions "tag" "dt" "_docker_tag"
  __add_docker_command_alias_completions "update" "dup" "_docker_update"
  __add_docker_command_alias_completions "unpause" "dun" "_docker_unpause"
}

# Mark a particular alias as a docker command for the purpose of adding shell
# completions, such as using `dim` for `docker images`.
__add_docker_command_alias_completions() {

  local cmd="${1}"
  local cmdAlias="${2}"
  local completionFunc="${3}"

  eval "alias ${cmdAlias}='docker ${cmd}'"
  eval "complete -F ${completionFunc} ${cmdAlias}"
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

  if isfunc "_completion_loader"; then
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

# Add sdkman completions to shell.
__add_sdkman_completions() {
  if [ "$(command -v sdk)" != "" ]; then
    . <(sdk env bash)
  fi
}

# Add asdf version manager command line completions for Bash.
__add_asdf_completions_for_bash() {
  local asdfDir="${ASDF_DIR:-${HOME}/.asdf}"
  local asdfCompletionFile="${asdfDir}/completions/asdf.bash"

  if [ -f "${asdfCompletionFile}" ]; then
    . "${asdfCompletionFile}"
  else
    warn "No ASDF completion file found at: ${BLUE}${asdfCompletionFile}${NC}"
  fi
}

# Add asdf version manager command line completions for Zsh.
__add_asdf_completions_for_zsh() {
  local asdfDir="${ASDF_DIR:-${HOME}/.asdf}"
  local asdfCompletionDir="${asdfDir}/completions"

  if [ -d "${asdfCompletionDir}" ]; then
    fpath=(${asdfCompletionDir} $fpath)
  else
    warn "No ASDF completion dir found at: ${BLUE}${asdfCompletionDir}${NC}"
  fi
}

# Add asdf version manager completions for Fish.
__add_asdf_completions_for_fish() {
  local asdfDir="${ASDF_DIR:-${HOME}/.asdf}"
  local asdfCompletionFile="${asdfDir}/completions/asdf.fish"

  if [ -d "${asdfDir}" ]; then
    . "${asdfCompletionFile}"
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
