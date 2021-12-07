#!/bin/sh

# Installs a given Lua rock for all versions of Lua that we care about.
# Can optionally provide extra CLI args for the luarocks command.
install_lua_package_for_all_versions() {
  local package="$1"
  shift
  local extra_args="$*"
  local eval_cmd
  local ver

  for ver in $(__get_lua_versions); do
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

# Clone, build, and install lua-language-server.
# Requires ninja and a C++-17 compiler and standard library.
install_lua_language_server() {
  local repoUrl="https://github.com/sumneko/lua-language-server.git"
  local repoName="$(basename "${repoUrl%.git}")"
  local repoDir="${WS:-${HOME}/workspace}/${repoName}"
  local executable=""
  local installPrefix="${HOME}"
  local installDir="${installPrefix}/bin"
  local wrapperScript="${installDir}/${repoName}"

  # Make sure the target directory for the symlink exists.
  if [ ! -d "${installDir}" ]; then
    err "${installDir} is not a directory."
    return 1
  fi

  if [ ! -d "${repoDir}" ]; then
    # Clone the repo if not already present.
    log_info "Cloning ${GREEN}${repoName}${NC}"
    git clone "${repoUrl}" "${repoDir}"
  else
    log_info "Updating ${GREEN}${repoName}${NC}"
    # Otherwise, update the existing repo.
    git -C "${repoDir}" checkout "$(defaultbranch "${repoDir}")"
    git pull
  fi

  log_info "Updating git submodules for ${GREEN}${repoName}${NC}"
  git -C "${repoDir}" submodule update --init --recursive

  (
    log_info "Generating Make files for ${repoName}"
    cd "${repoDir}/3rd/luamake"
    ./compile/install.sh
  )

  (
    log_info "Compiling ${repoName}"
    cd "${repoDir}"
    ./3rd/luamake/luamake rebuild
  )

  # This project puts the compiled executable into a directory named after the
  # system on which it is installed (i.e. "windows", "macOs", "linux", etc.).
  # We want to dynamically retrieve the path to the executable here.
  executable="$(find "${repoDir}" -type f -name "*${repoName}" |
    head -1)"

  if [ ! -x "${executable}" ]; then
    err "Could not locate ${repoName} executable."
    return 1
  fi

  log_info "Installing wrapper script"
  command cat <<EOF >"${wrapperScript}"
#!/bin/sh

exec "${executable}" "${repoDir}/main.lua"
EOF

  chmod +x "${wrapperScript}"
}

install_json4lua() {
  install_lua_package_for_all_versions "json4lua" \
    "--server=http://rocks.moonscript.org/manifests/amrhassan"
}

install_lua_packages() {
  local package

  log_info "Installing Lua packages from standard respositories"
  while IFS="" read -r package || [ -n "$package" ]; do
    install_lua_package_for_all_versions "${package}"
  done <"${LUA_PACKAGES_FILE}"

  log_info "Installing special lua packages"
  install_lua_language_server
  install_json4lua
}

# Get all installed Lua versions
__get_lua_versions() {
  # We want to find and filter out our versions of Lua.
  # Versions before 5.3 are not very useful to us.
  compgen -c | grep '^lua[0-9].[0-9]$' | sort -u | sed 's/^lua//' |
    grep -v '5.[1-2]' |
    tr '\n' ' '
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
