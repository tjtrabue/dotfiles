#!/bin/sh

# Make sure all dotfile symlinks are present in $HOME directory
dotsync() {
  local userConfigDir="${USER_CONF:-${HOME}/.config}"
  local dotfilesLink="${DOTFILES_LINK:-${DOTFILES_HOME}/link}"

  # Make sure that ~/.config exists
  mkdir -p "${userConfigDir}"

  # Re-link all basic dotfiles
  find "${dotfilesLink}/home" -maxdepth 1 -mindepth 1 -type f \
    -exec ln -sf -t "${HOME}" '{}' \;

  # Re-link all files and directories in the ~/.config directory
  find "${dotfilesLink}/config" -maxdepth 1 -mindepth 1 \
    -exec ln -sfn -t "${userConfigDir}" '{}' \;

  # Finish by removing broken symlinks.
  rmbsyml "${HOME}"
  rmbsyml "${userConfigDir}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
