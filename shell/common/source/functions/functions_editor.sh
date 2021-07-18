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

# Open a number of files containing text matching an input pattern in the
# configured $EDITOR program.
# Optionally takes a second string of CLI options for the search command.
grepedit() {
  local searchCommand="grep -l"
  # The regex to search files for.
  local searchPattern="${1}"
  # Extra CLI options for the search command (not required).
  local extraOpts="${2}"

  if [ -z "${searchPattern}" ]; then
    err "No search pattern provided to patedit."
    return 1
  fi

  if [ -x "$(command -v rg)" ]; then
    # Use ripgrep if available.
    searchCommand="rg -l"
  elif [ -x "$(command -v ag)" ]; then
    # In lieu of ripgrep, the_silver_searcher is still better than grep.
    searchCommand="ag -l"
  fi

  # Add extra CLI args to search command if they were provided.
  [ -n "${extraOpts}" ] && searchCommand="${searchCommand} ${extraOpts}"

  # Add the search pattern to the end of the search command.
  searchCommand="${searchCommand} -- '${searchPattern}'"

  # Open the files in the configured editor
  eval "edit "$(eval "${searchCommand}")""
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
