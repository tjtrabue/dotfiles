#!/usr/bin/env zsh

# Conditionally set $ZDOTDIR {{{
# Where we want to look for our Zsh dotfiles.
DESIRED_ZDOTDIR="${HOME}/.zsh"

# We may want to ignore fancy shell settings under certain circumstances,
# opting instead to start the shell with a minimal environment.
if [ "${USE_LEAN_PROFILE}" = 1 ] || [ "${USE_LEAN_PROFILE}" = "true" ]; then
  # Make sure ZDOTDIR is unset if we want a lean profile.
  export ZDOTDIR=""
  # Only pull in basic definitions
  LEAN_PROFILE="${HOME}/.lean_profile"
  [ -s "${LEAN_PROFILE}" ] && . "${LEAN_PROFILE}"
  unset LEAN_PROFILE
elif [ -d "${DESIRED_ZDOTDIR}" ] || [ -h "${DESIRED_ZDOTDIR}" ]; then
  # Set the user's dotfile dir for Zsh, which will hold startup files for Zsh,
  # such as .zprofile, .zshrc, etc.
  export ZDOTDIR="${DESIRED_ZDOTDIR}"
fi

unset DESIRED_ZDOTDIR
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
