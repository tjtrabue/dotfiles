#!/bin/sh

# batdiff aliases:
if [ -x "$(command -v batdiff)" ]; then
  if [ -x "$(command -v delta)" ]; then
    # Use the delta command to view diffs when available.
    alias batdiff="batdiff --delta"
  fi
fi

# vim:foldenable:foldmethod=marker:foldlevel=0