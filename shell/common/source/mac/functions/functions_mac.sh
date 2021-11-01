#!/bin/sh

# Prepare a mac computer for development.
prepare_mac() {
  install_mac_developer_tools
  install_homebrew
  create_gnu_cli_tool_aliases_for_mac
}

# Mac operating systems need the user to install a suite of developer tools
# before any useful software development can be performed.
install_mac_developer_tools() {
  # Make sure developer tools are installed
  xcode-select --install >>/dev/null 2>&1 || :
}

# Install the homebrew package manager for macOS.
install_homebrew() {
  if [ ! -x "$(command -v brew)" ]; then
    /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

# Ensure that homebrew knows where to find recipes.
tap_brew_casks() {
  log_info "Tapping homebrew casks"
  brew tap d12frosted/emacs-plus
}

# Install all homebrew packages
install_mac_packages() {
  local package

  tap_brew_casks

  if [ ! -f "${BREW_PACKAGES_FILE}" ]; then
    err "No homebrew packages file found at: ${BREW_PACKAGES_FILE}"
    return 1
  fi

  xargs brew install <"${BREW_PACKAGES_FILE}"
}

# Install the most important GNU tools on macOS.
install_gnu_cli_tools_for_mac() {
  brew install coreutils
  brew install binutils
  brew install diffutils
  brew install ed
  brew install findutils
  brew install gawk
  brew install global
  brew install gnu-getopt
  brew install gnu-indent
  brew install gnu-sed
  brew install gnu-tar
  brew install gnu-which
  brew install gnutls
  brew install grep
  brew install gzip
  brew install screen
  brew install tmux
  brew install watch
  brew install wdiff
  brew install wget
}

# macOS uses BSD versions of UNIX command line tools by default. The GNU
# versions are much more powerful and up-to-date, so we want to use them
# instead. The problem is, the GNU CLI tools are all prefixed with "g" on macOS.
# The non-prefixed versions are available, but not used by default. Thus, we
# need to add these non-prefixed versions of the tools to our PATH, and give
# them priority over the BSD tools.
create_gnu_cli_tool_aliases_for_mac() {
  local brewPrefix="$(brew --prefix)"
  local pathFile="${PATH_FILE:-${HOME}/.path}"

  log_info "Adding GNU CLI tools to \$PATH"
  log_debug "Updating locate database"

  # Update the locate database
  if [ -x "$(command -v gupdatedb)" ]; then
    sudo gupdatedb
  elif [ -x "/usr/libexec/locate.updatedb" ]; then
    sudo /usr/libexec/locate.updatedb
  fi

  log_debug "Writing gnubin directories to PATH file"
  glocate --regex "^${brewPrefix}.*gnubin\$" >> "${pathFile}"
  spath
  rmduplines "${pathFile}"
  export_path
}

# vim:foldenable:foldmethod=indent:foldlevel=0:foldnestmax=1
