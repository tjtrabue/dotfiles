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

  link_roswell_init_file

  install_roswell_scripts
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
install_roswell_scripts() {
  log_info "Installing important roswell scripts"
  ros install qlot \
    cl-project \
    clack \
    cxxxr/lem \
    cxxxr/cl-lsp
}

# Install all Common Lisp packages needed globally.
install_common_lisp_packages() {
  {
    if [ ! -x "$(command -v ros)" ]; then
      warn "Roswell executable not found; installing"
      install_roswell
    fi
  } &&
    {
      log_info "Installing Common Lisp packages..."
      install_packages_from_file_with_tool "ros install" \
        "${COMMON_LISP_PACKAGES_FILE}"
    }
}

# Link our Roswell initialization file to ~/.roswell/init.lisp.
# The code in this file gets executed whenever Roswell starts a new REPL.
link_roswell_init_file() {
  local roswellLinkDir="${DOTFILES_LINK}/roswell"
  local roswellInitFileName="init.lisp"
  local roswellInitFile="${roswellLinkDir}/${roswellInitFileName}"
  local roswellHomeDir="${HOME}/.roswell"
  local roswellInitTarget="${roswellHomeDir}/${roswellInitFileName}"

  if [ ! -d "${roswellHomeDir}" ]; then
    err "No Roswell home dir found at: ${BLUE}${roswellHomeDir}${NC}"
    return 1
  fi

  if [ -f "${roswellInitTarget}" ] && [ ! -h "${roswellInitTarget}" ]; then
    log_info "Backing up existing initialization file:" \
      "${BLUE}${roswellInitTarget}${NC}"
    mv "${roswellInitTarget}"{,.bak}
  fi

  log_info "Linking Roswell initialization file:" \
    "${BLUE}${roswellInitTarget}${NC}"
  ln -sf -t "${roswellHomeDir}" "${roswellInitFile}"
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
