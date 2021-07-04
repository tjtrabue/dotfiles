#!/bin/sh

# Install the Elixir language server protocol implementation.
# The install prefix for the libraries and executables may be provided as an
# optional argument.
install_elixir_ls() {
  local installPrefix="${1:-${HOME}}"

  __clone_elixir_ls &&
  __build_elixir_ls &&
  __install_elixir_ls "${installPrefix}"
}

# Clone the elixir-ls project.
__clone_elixir_ls() {
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsRepo="${workspace}/elixir-ls"
  local elixirLsGitUrl="https://github.com/elixir-lsp/elixir-ls.git"

  if [ ! -d "${elixirLsRepo}" ]; then
    (
      log_info "Cloning ElixirLS repository to: ${elixirLsRepo}"
      cd "${workspace}"
      git clone "${elixirLsGitUrl}"
    )
  else
    warn "ElixirLS project already found at: ${elixirLsRepo}"
  fi
}

# Compile the elixir-ls project and generate Mix archives.
__build_elixir_ls() {
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsRepo="${workspace}/elixir-ls"

  (
    log_info "Building ElixirLS"
    cd "${elixirLsRepo}"
    export MIX_ENV="prod"
    export MIX_HOME="${HOME}/.mix"
    mix local.hex --force
    mix deps.get
    mix compile
  )
}

# Install the elixis-ls executables.
__install_elixir_ls() {
  # The directory containing the lib/ and bin/ directories for elixir-ls.
  local installPrefix="${1:-${HOME}}"
  local workspace="${WS:-${HOME}/workspace}"
  local elixirLsName="elixir-ls"
  local elixirLsRepo="${workspace}/${elixirLsName}"
  local installLib="${installPrefix}/lib"
  local installBin="${installPrefix}/bin"

  (
    cd "${elixirLsRepo}"
    export MIX_ENV="prod"
    install -dm0755 "${installLib}/${elixirLsName}"
    mix elixir_ls.release -o "${installLib}/${elixirLsName}"

    install -dm0755 "${installBin}"
    # Create an executable script to launch the language server
    cat <<EOF >"${installBin}/elixir-ls"
#!/bin/sh
exec ${installLib}/${elixirLsName}/language_server.sh
EOF
    chmod 755 "${installBin}/elixir-ls"

    # Create an executable script to launch a debugger program for the LS.
    cat <<EOF >"${installBin}/elixir-ls-debug"
#!/bin/sh
exec ${installLib}/${elixirLsName}/debugger.sh
EOF
    chmod 755 "${installBin}/elixir-ls-debug"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
