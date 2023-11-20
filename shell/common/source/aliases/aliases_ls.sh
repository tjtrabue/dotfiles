#!/bin/sh

# Use a fancier ls alternative if available.
if [ -x "$(command -v eza)" ]; then
  alias ls="eza"
elif [ -x "$(command -v exa)" ]; then
  alias ls="exa"
fi

# vim:foldenable:foldmethod=marker:foldlevel=0
