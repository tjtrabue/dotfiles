#!/usr/bin/env zsh

# `clipboard` is a custom CLI wrapper for interacting with the system clipboard.
# For Zsh, we want to make `cb` a global alias so that we can use it in
# pipelines. Unfortunately, Bash does not support global aliases.
alias -g cb="clipboard"
