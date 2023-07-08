#!/bin/sh

# Install the helm "package manager" for Kubernetes deployments.
install_k8s_helm() {
  local helmInstallerScriptUrl="https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3"

  log_info "Installing Helm for Kubernetes"
  curl -sL "${helmInstallerScriptUrl}" | bash

  init_helm_repos
}

# Initialize a new helm installation with basic, project-agnostic repositories.
init_helm_repos() {
  log_info "Configuring Helm repositories"
  helm repo add bitnami https://charts.bitnami.com/bitnami
}

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
