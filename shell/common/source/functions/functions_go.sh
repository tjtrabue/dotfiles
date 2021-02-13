#!/bin/sh

# Install packages managed by the go-lang language.
install_go_packages() {
  if [ "$(command -v parallel)" != "" ]; then
    parallel --bar go get :::: "$GO_PACKAGES_FILE"
  else
    xargs go get <"$GO_PACKAGES_FILE"
  fi

  # These packages may require special variables and settings and as such
  # are not suitable to placement in a centralized package file.

  # shfmt
  GO111MODULE=on go get mvdan.cc/sh/v3/cmd/shfmt
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
