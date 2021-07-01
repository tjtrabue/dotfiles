#!/bin/sh

# Fuzzy finds files and opens them in the configured editor.
ctrlp() {
  local filesToEdit=()
  local previewCmd="cat -n 500"

  # Use bat as the previewer if available.
  if [ -x "$(command -v bat)" ]; then
    previewCmd="bat --color=always --style=numbers --line-range=:500 {}"
  fi

  # Prioritized list of fuzzy search tools used to find files.
  if [ -x "$(command -v fzf)" ]; then
    filesToEdit=($(fzf --preview "${previewCmd}"))
  elif [ -x "$(command -v fzy)" ]; then
    filesToEdit=($(fd -t f '.' . | fzy))
  fi

  if [ -n "${filesToEdit[*]}" ]; then
    edit "${filesToEdit[@]}"
  fi
}

# Open a "source" file in the configured editor.
esf() {
  pushd "$DOTFILES_HOME/bash/source" &>/dev/null
  ctrlp
  popd &>/dev/null
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
