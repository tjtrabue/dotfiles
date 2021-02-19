#!/bin/sh

# Make sure that all sessions have access to user's executables
NEW_PATH="${HOME}/bin"
NEW_PATH="${HOME}/.local/bin:${NEW_PATH}"
PATH="${NEW_PATH}:${PATH}"
export PATH
unset NEW_PATH

# Pull in environment variables to all shells.
[ -f "${HOME}/.vars" ] && . "${HOME}/.vars"
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
