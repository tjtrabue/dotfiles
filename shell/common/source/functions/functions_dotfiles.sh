#!/bin/sh

# Make sure all dotfile symlinks are present in $HOME directory
dotsync() {
  local dotfilesLink="${DOTFILES_LINK:-${DOTFILES_HOME}/link}"

  # Re-link all basic dotfiles
  find "${dotfilesLink}/home" -maxdepth 1 -mindepth 1 -type f \
    -exec ln -sf -t "${HOME}" '{}' \;

  # Finish by removing broken symlinks.
  rmbsyml "${HOME}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
