#!/usr/bin/env bash

# Conditionally source ~/.bashrc {{{
# We may want to ignore fancy shell settings under certain circumstances,
# opting instead to start the shell with a minimal environment.
if [ "${SLIM_PROFILE}" = 1 ] || [ "${SLIM_PROFILE}" = "true" ]; then
  # Only pull in basic definitions.
  LEAN_PROFILE="${HOME}/.lean_profile"
  [ -s "${LEAN_PROFILE}" ] && . "${LEAN_PROFILE}"
  unset LEAN_PROFILE
elif [ -s "${HOME}/.bashrc" ]; then
  # All Bash configuration is stored in ~/.bashrc
  # This makes managing Bash much easier. Try to centralize as much
  # configuration as possible.
  . "${HOME}/.bashrc"
fi
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
