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

# vim:foldenable:foldmethod=indent:foldnestmax=1
