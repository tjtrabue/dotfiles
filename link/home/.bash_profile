#!/usr/bin/env bash

# Conditionally source ~/.bashrc {{{
# We may want to ignore fancy shell settings under certain circumstances,
# opting instead to start the shell with a minimal environment.
if [ "${USE_LEAN_PROFILE}" = 1 ] || [ "${USE_LEAN_PROFILE}" = "true" ]; then
  # Only pull in basic definitions if we want a lean profile.
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
