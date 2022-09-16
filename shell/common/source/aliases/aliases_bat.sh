#!/bin/sh

# batdiff aliases:
if [ -x "$(command -v batdiff)" ]; then
  # Always use the delta command to view diffs.
  alias batdiff="batdiff --delta"
fi

# vim:foldenable:foldmethod=marker:foldlevel=0