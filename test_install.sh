#!/usr/bin/env bash

# Test the install.sh script by installing all dotfiles to a "test" directory.

if [ -d "test" ]; then
  rm -rf "test"
fi

mkdir -p "test"

./install.sh -vv --force --fake-home "test"
