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
  # The parent of the "bin" directory containing the installed binary.
  local installPrefix="${HOME}/.local"
  # Where we want to install clojure-lsp
  local installDir="${installPrefix}/bin"

  log_info "Installing Clojure LSP to: ${BLUE}${installDir}${NC}"

  # Make sure our install directory exists before proceeding.
  mkdir -p "${installDir}"

  (
    curl -sLO "${clojureLspDownloadUrl}" &&
      chmod +x "${clojureLspInstaller}" &&
      ./"${clojureLspInstaller}" --dir "${installDir}" &&
      rm -f "${clojureLspInstaller}"
  )
}

# Install the clj-kondo Clojure linting tool.
install_clj_kondo() {
  local cljKondoInstallerUrl="https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo"
  local cljKondoInstaller="$(basename "${cljKondoInstallerUrl}")"

  curl -sLO "${cljKondoInstallerUrl}"
  chmod +x "${cljKondoInstaller}"
  ./"${cljKondoInstaller}"
  rm -f "${cljKondoInstaller}"
}

# Wrapper for the Leiningen command line tool that supports readline
# configuration to make using the Clojure REPL more joyful.
wlein() {
  local commandString="$*"
  local leinCommand="rlwrap --always-readline lein ${commandString}"

  if [ ! -x "$(command -v lein)" ]; then
    err "${GREEN}lein${NC} command line tool not installed."
    return 1
  fi

  eval "${leinCommand}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
