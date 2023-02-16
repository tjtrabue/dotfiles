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
#
# If installing on Windows for WSL, see the following page for instructions:
# https://github.com/hubisan/emacs-wsl
#
# As well as the following article on using Linux GUI apps on Windows:
# https://learn.microsoft.com/en-us/windows/wsl/tutorials/gui-apps
install_latest_emacs() {
  local emacsGitUrl="https://github.com/emacs-mirror/emacs.git"
  local emacsDest="${WS:-${HOME}/workspace}/emacs"

  mkdir -p "$(dirname "${emacsDest}")"
  log_info "Cloning/Updating Emacs Git Repo"
  clone_or_update_git_repo "${emacsGitUrl}" "${emacsDest}"
  (
    log_info "Installing latest Emacs for Ubuntu" && \
    cd "${emacsDest}" && \
    ./autogen.sh && \
    ./configure --with-json --with-tree-sitter --with-xwidgets \
      --with-imagemagick --with-mailutils --with-native-compilation=aot \
      --with-pgt && \
    make -j$(nproc) && \
    sudo make install
  )
}

# Install the lazygit commandline Git porcelain for an Ubuntu distribution.
install_lazygit_ubuntu() {
  local lazygitVersion="$(curl -s \
    "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" |
    grep -Po '"tag_name":\s*"v\K[^"]*'
  )"
  (
    log_info "Downloading latest lazygit version" && \
    curl -sL -o lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${lazygitVersion}_Linux_x86_64.tar.gz" && \
    tar -zxvf lazygit.tar.gz lazygit && \
    sudo install lazygit /usr/local/bin && \
    rm -f 'lazygit.tar.gz' 'lazygit'
  )
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
