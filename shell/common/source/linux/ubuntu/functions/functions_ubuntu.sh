#!/bin/sh

# Install all Ubuntu APT packages.
install_ubuntu_packages() {
  local ubuntuPackagesFile="${DOTFILES_PACKAGES_DIR}/ubuntu_packages.txt"

  log_info "Installing Ubuntu packages from: ${BLUE}${ubuntuPackagesFile}${NC}"
  eval "sudo apt update && sudo apt install -y" \
    "$(build_package_cmd_line_from_file "${ubuntuPackagesFile}")"
}

# Install the bat commandline utility to replace cat(1).
install_bat_ubuntu() {
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

# Install GitHub command line tools for an Ubuntu distribution.
install_gh_tools_ubuntu() {
  type -p curl >/dev/null || sudo apt install curl -y
  curl -fsSL 'https://cli.github.com/packages/githubcli-archive-keyring.gpg' |
  sudo dd of='/usr/share/keyrings/githubcli-archive-keyring.gpg' && \
  sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg && \
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" |
  sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
  sudo apt update && \
  sudo apt install gh -y
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
