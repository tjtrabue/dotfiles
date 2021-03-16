#!/bin/sh

# Use the `bat` instead of `cat` when possible. `bat` is a much improved tool
# that supports syntax highlighting, version control information, and more.
if [ -x "$(command -v bat)" ]; then
  alias cat="bat"
fi

# vim:foldenable:foldmethod=marker:foldlevel=0
