#!/bin/sh

# Download the Erlang project bootstrap Make template.
download_erlang_mk() {
  local erlangMkUrl="https://erlang.mk/erlang.mk"

  log_info "Downloading Erlang project Make template"
  curl -sSLO "${erlangMkUrl}"
}

# Turn the current working directory into an Erlang project.
bootstrap_erlang_project() {
  local erlangMkFile="erlang.mk"

  if [ ! -f "${erlangMkFile}" ]; then
    download_erlang_mk
  fi

  log_info "Bootstrapping new Erlang project"
  make -f "${erlangMkFile}" bootstrap bootstrap-rel
}

# Install the Erlang language server.
install_erlang_ls() {
  local erlangLsGitRepoUrl="https://github.com/erlang-ls/erlang_ls.git"
  local erlangLsDest="${WS:-${HOME}/workspace}/erlang_ls"
  # The directory containing the "bin/" directory where the erlang_ls executable
  # script will be installed.
  local installPrefix="${HOME}/.local"

  clone_or_update_git_repo "${erlangLsGitRepoUrl}" "${erlangLsDest}"

  log_info "Installing erlang_ls"
  mkdir -p "${installPrefix}"
  (
    cd "${erlangLsDest}"
    make
    PREFIX="${installPrefix}" make -e install
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1