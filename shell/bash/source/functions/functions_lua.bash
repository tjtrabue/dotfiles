#!/usr/bin/env bash

# Get all installed Lua versions
__get_lua_versions() {
  # We want to find and filter out our versions of Lua.
  # Versions before 5.3 are not very useful to us.
  compgen -c | grep '^lua[0-9].[0-9]$' | sort -u | sed 's/^lua//' \
    | grep -v '5.[1-2]' \
    | tr '\n' ' '
}

# Installs a given Lua rock for all versions of Lua that we care about.
# Can optionally provide extra CLI args for the luarocks command.
install_lua_package_for_all_versions() {
  local package="$1"
  shift
  local extra_args="$*"
  local eval_cmd
  local versions
  # The versions of Lua to target
  read -ra versions <<<"$(__get_lua_versions)"

  for ver in "${versions[@]}"; do
    log_info "Installing ${GREEN}${package}${NC} for Lua" \
      "version ${BLUE}${ver}${NC}"
    eval_cmd="luarocks install --local --lua-version ${ver}"
    if [ -n "${extra_args}" ]; then
      eval_cmd="${eval_cmd} ${extra_args}"
    fi
    eval_cmd="${eval_cmd} ${package}"
    eval "${eval_cmd}"
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
