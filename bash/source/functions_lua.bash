#!/usr/bin/env bash

install_lua_lsp() {
 luarocks install --local --server=http://luarocks.org/dev lua-lsp
}

install_lua_packages() {
  local package;

  log_info "Installing Lua packages from standard respositories"
  while read -r package || [ -n "$package" ]; do
    luarocks install --local "$package"
  done < "$LUA_PACKAGES_FILE"

  log_info "Installing lua-lsp from special repository"
  install_lua_lsp
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
