#!/usr/bin/env bash

# Installs a given Lua rock for all versions of Lua that we care about.
# Can optionally provide extra CLI args for the luarocks command.
install_lua_package_for_all_versions() {
  local package="${1:-''}"
  shift
  local extra_args="${*:-''}"
  # The versions of Lua to target
  local versions=("5.3" "5.4")
  local ver

  for ver in "${versions[@]}"; do
    log_info "Installing ${package} for Lua version ${ver}"
    if [ -n "$(command -v "lua${ver}")" ]; then
      eval "luarocks install --local --lua-version" "${ver}" \
        "${extra_args}" "${package}"
    else
      warn "Lua version ${ver} not found. Skipping."
    fi
  done
}

install_lua_lsp() {
  install_lua_package_for_all_versions "lua-lsp" \
    "--server=http://luarocks.org/dev"
}

install_json4lua() {
  install_lua_package_for_all_versions "json4lua" \
    "--server=http://rocks.moonscript.org/manifests/amrhassan"
}

install_lua_packages() {
  local package

  log_info "Installing Lua packages from standard respositories"
  while read -r package || [ -n "$package" ]; do
    install_lua_package_for_all_versions "${package}"
  done <"$LUA_PACKAGES_FILE"

  log_info "Installing special lua packages"
  install_lua_lsp
  install_json4lua
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
