#!/usr/bin/env bash

# All Bash configuration is stored in ~/.bashrc
# This makes managing Bash much easier. Try to centralize as much configuration
# as possible.
[ -s "${HOME}/.bashrc" ] && source "${HOME}/.bashrc"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/thomastrabue/.sdkman"
[[ -s "/Users/thomastrabue/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/thomastrabue/.sdkman/bin/sdkman-init.sh"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
