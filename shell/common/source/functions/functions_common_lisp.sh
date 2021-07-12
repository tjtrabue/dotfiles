#!/bin/sh

# Roswell is a Common Lisp environment and lifecycle manager that acts as both a
# means for installing and managing Common Lisp implementations as well as a
# tool for Common Lisp application development.
install_roswell() {
  local os="$(getdistro)"

  case "${os}" in
    "Darwin")
      __install_roswell_mac
      ;;
    "Arch Linux")
      __install_roswell_arch_linux
      ;;
    *)
      __install_roswell_generic
      ;;
  esac

}

# Install all relevant Common Lisp distributions.
install_common_lisps() {
  if [ -x "$(command -v ros)" ]; then
    roswell install sbcl-bin
  else
    err "ros (roswell) command line tool not installed."
    return 1
  fi
}

# Install the LSP server for Common Lisp.
# You need roswell in order to install this.
install_common_lisp_lsp() {
  ros install cxxxr/lem cxxxr/cl-lsp
}

# Install roswell with homebrew.
__install_roswell_mac() {
  log_info "Installing roswell for macOS"
  brew install roswell
}

# Use the AUR helper to install roswell.
__install_roswell_arch_linux() {
  log_info "Installing roswell for Arch Linux from AUR"
  aurhinc roswell
}

# Generic installation instructions for roswell
__install_roswell_generic() {
  local workspace="${WS:-${HOME}/workspace}"
  local roswellGitUrl="https://github.com/roswell/roswell.git"
  local roswellGitBranch="release"
  local roswellRepo="${workspace}/roswell"
  # Install roswell locally
  local roswellInstallPrefix="${HOME}/.local"

  log_info "Installing roswell for generic *NIX systems"

  if [ ! -d "${roswellRepo}" ]; then
    git clone -b "${roswellGitBranch}" "${roswellGitUrl}" "${roswellRepo}"
  fi

  (
    cd "${roswellRepo}" &&
    sh bootstrap &&
    ./configure --prefix="${roswellInstallPrefix}/" &&
    make &&
    make install &&
    ros setup
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
