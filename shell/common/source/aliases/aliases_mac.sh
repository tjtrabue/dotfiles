#!/bin/sh

# Homebrew {{{

# Update formulae and then upgrade them in one fell swoop.
alias brewup="brew update && brew upgrade"

# Clean Homebrew caches
alias bcleanup="brew cleanup --prune=all"
# }}}

if [ -x "$(command -v gupdatedb)" ]; then
  # Updating the locate database on macOS will fail unless we exclude the
  # /Volumes and /System directories.
  alias updatedb="gupdatedb --localpaths=/ --prunepaths='/Volumes /System'"
fi

# Emacs inherits several environment variables whose values may prove to be
# problematic.
alias emacs="unset CPATH; unset LIBRARY_PATH; emacs"

# vim:foldenable:foldmethod=marker:foldlevel=0
