#!/bin/sh

# Install Clojure's `clj` and `clojure` commandline tools.
install_clojure_cli_tools() {
  local os="$(getdistro)"

  log_info "Installing Clojure CLI tools"
  case "${os}" in
    "Darwin")
      __install_clojure_cli_tools_mac
      ;;
    *)
      __install_clojure_cli_tools_linux
      ;;
  esac
}

__install_clojure_cli_tools_mac() {
  log_info "Installing Clojure CLI tools for Mac (Darwin)"
  brew install "clojure/tools/clojure"
}

__install_clojure_cli_tools_linux() {
  # Location from which to download Clojure CLI tools.
  local clojureCliDownloadUrl="https://download.clojure.org/install/linux-install.sh"
  # Name of the downloaded install script.
  local clojureCliInstaller="$(basename "${clojureCliDownloadUrl}")"

  log_info "Installing Clojure CLI tools for Linux"
  {
    curl -O "${clojureCliDownloadUrl}" &&
    chmod +x "${clojureCliInstaller}" &&
    sudo ./"${clojureCliInstaller}" &&
    rm -f "${clojureCliInstaller}"
  }
}

# Install the Clojure Language Server.
install_clojure_lsp() {
  # Where to get the install script
  local clojureLspDownloadUrl="https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install"
  # The downloaded install script
  local clojureLspInstaller="$(basename "${clojureLspDownloadUrl}")"
  # Where we want to install clojure-lsp
  local installDir="${HOME}/.local/bin"

  log_info "Installing Clojure LSP"
  (
    curl -sLO "${clojureLspDownloadUrl}" &&
    chmod +x "${clojureLspInstaller}" &&
    ./"${clojureLspInstaller}" --dir "${installDir}" &&
    rm -f "${clojureLspInstaller}"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
