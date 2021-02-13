#!/bin/sh

ranger-cd() {
  local tempFile="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
  ranger --choosedir="$tempFile"
  local lastDir="$(cat "$tempFile")"
  cd "$lastDir"
  rm -f "$tempFile"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
