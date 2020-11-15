#!/usr/bin/env bash

# Fuzzy finds files and opens them in the configured editor.
ctrlp() {
  local filesToEdit=($(fzf));
  if [[ -n "${filesToEdit[*]}" ]]; then
    eval "edit" "${filesToEdit[@]}";
  fi
}

# Open a "source" file in the configured editor.
esf() {
  pushd "$DOTFILES_HOME/bash/source" &> /dev/null;
  ctrlp;
  popd &> /dev/null;
}

# Remove all Vim swap files
rmvimswap() {
  rm -f "${VIM_SWAPS}"/*
}

# Remove all Vim backup files
rmvimbackup() {
  rm -f "${VIM_BACKUPS}"/*
}

# vim:foldenable:foldmethod=indent