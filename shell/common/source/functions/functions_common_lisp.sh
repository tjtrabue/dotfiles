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
      err "Cannot install roswell for OS: ${os}"
      return 1
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
  brew install roswell
}

# Use the AUR helper to install roswell.
__install_roswell_arch_linux() {
  aurhinc roswell
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
