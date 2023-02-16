#!/bin/sh

# Install the bat commandline utility to replace cat(1).
install_bat() {
  local batInstallDest="${HOME}/.local/bin"

  sudo apt install bat
  # Due to a naming conflict, Ubuntu installs bat as batcat, so we should
  # symlink the executable to one named "bat" so that it will behave as
  # expected.
  if [ -x "$(command -v batcat)" ]; then
    mkdir -p "${batInstallDest}"
    ln -sf "$(command -v batcat)" "${batInstallDest}/bat"
  fi
}

# Intall the latest version of Emacs from source.
install_latest_emacs() {
  local emacsGitUrl="https://github.com/emacs-mirror/emacs.git"
  local emacsDest="${WS}/emacs"

  log_info "Installing Emacs for Ubuntu"
  git clone "${emacsGitUrl}" "${emacsDest}"
  (
    cd "${emacsDest}" && \
    ./autogen.sh && \
    ./configure --with-json --with-tree-sitter --with-xwidgets \
      --with-imagemagick --with-mailutils --with-native-compilation=aot \
      --with-pgt && \
    make -j$(nproc) && \
    sudo make install
  )
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
