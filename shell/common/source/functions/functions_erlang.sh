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

# vim:foldenable:foldmethod=indent:foldnestmax=1