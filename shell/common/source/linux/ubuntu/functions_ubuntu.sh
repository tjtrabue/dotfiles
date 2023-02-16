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

# vim:foldenable:foldmethod=indent::foldnestmax=1
