#!/bin/sh

# Add shell completion for helm.
init_helm_completions() {
  local userShell="$(basename "${SHELL}")"

  if [ -z "$(command -v helm)" ]; then
    err "helm command line tool not found on \$PATH"
    return 1
  fi

  case "${userShell}" in
  "bash")
    __init_helm_completions_bash
    ;;
  "zsh")
    __init_helm_completions_zsh
    ;;
  esac
}

__init_helm_completions_bash() {
  local bashCompletionDir="${HOME}/.bash_completion.d"
  local helmCompletionFile="${bashCompletionDir}/helm"

  log_info "Initializing helm completions for bash"

  mkdir -p "${bashCompletionDir}"
  helm completion bash >"${helmCompletionFile}"
  soure <(helm completion bash)
}

__init_helm_completions_zsh() {
  log_info "Initializing helm completions for zsh"
  helm completion zsh >"${fpath[1]}/_helm"
  source <(helm completion zsh)
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
