#!/usr/bin/env bash

# Make sure all dotfile symlinks are present in $HOME directory
dotsync() {
  find "$DOTFILES_HOME/link/home" -maxdepth 1 -mindepth 1 -type f \
    -exec ln -sf '{}' "$HOME/" \;
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
