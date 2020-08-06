#!/usr/bin/env bash

install_lua_lsp() {
  luarocks install --local --server=http://luarocks.org/dev lua-lsp
}

install_json4lua() {
  luarocks install --server=http://rocks.moonscript.org/manifests/amrhassan \
    --local json4Lua
}

install_lua_packages() {
  local package

  log_info "Installing Lua packages from standard respositories"
  while read -r package || [ -n "$package" ]; do
    luarocks install --local "$package"
  done <"$LUA_PACKAGES_FILE"

  log_info "Installing special lua packages"
  install_lua_lsp
  install_json4lua
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
