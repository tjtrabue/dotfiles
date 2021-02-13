#!/bin/sh

# Locate a given font via an input regular expression
findfont() {
  local pattern="$1"
  eval "fc-list | grep -E -i '$pattern' | less"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
