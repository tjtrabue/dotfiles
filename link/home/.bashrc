#!/usr/bin/env bash

# Make sure that file creation occurs with the normal permissions.
umask 022

# Where the magic happens:
DOTFILES_HOME="${HOME}/.dotfiles"
DOTFILES_COMMON="${DOTFILES_HOME}/shell/common"

# Additional $PATH settings {{{
# Add sbin to PATH if it exists
[ -d "/usr/local/sbin" ] && export PATH="/usr/local/sbin:$PATH"
[ -d "/usr/sbin" ] && export PATH="/usr/sbin:$PATH"
# Add dotfiles binaries to the path:
export PATH="$DOTFILES_HOME/bash/bin:$PATH"
# No need to export all additional directories here because `spath` takes care
# of that by exporting all directories listed in ~/.path
# }}}

# Pull in common shell aliases/functions as soon as possible.
. "${DOTFILES_COMMON}/source/common.sh"

# Shell completion {{{
# Include extra CLI completions if available.
add_shell_completions
# }}}

# Change ls colors (dircolors) {{{
dircolorsFile="$DIRCOLORS_DIR/tomorrow-night-eighties.dircolors"
if [ -f "$dircolorsFile" ]; then
  eval "$(dircolors "$dircolorsFile")"
fi
unset dircolorsFile
# }}}

# Special Prompts {{{
POWERLINE_BASH_BINDINGS="$(find "${HOME}/.local/lib/" -type f \
  -regextype 'posix-extended' \
  -regex '.*python3.[0-9]+.*bindings/bash/powerline.sh' |
  sort -V |
  tail -1)"

if [ -x "$(command -v starship)" ]; then
  # The starship prompt is a beautiful, informative, cross-shell prompt
  eval "$(starship init bash)"
elif [ -f "${POWERLINE_BASH_BINDINGS}" ]; then
  # Default to powerline if no other prompt installed
  powerline-daemon -q
  export POWERLINE_BASH_CONTINUATION=1
  export POWERLINE_BASH_SELECT=1
  . "${POWERLINE_BASH_BINDINGS}"
fi
# }}}

# Make sure to set prompt to something simple for Emacs TRAMP {{{
if [ "$TERM" == "dumb" ]; then
  PS1="tramp $ "
fi
# }}}

# Set transparency for xterm {{{
if [ -n "$XTERM_VERSION" ] && [ "$(command -v transset-df)" != "" ]; then
  transset-df --id "$WINDOWID" >/dev/null
fi
# }}}

# Load fzf keybindings {{{
if [ -f "${HOME}/.fzf.bash" ]; then
  . "${HOME}/.fzf.bash"
fi
# }}}

# These icons are supplied by the icons-in-terminal project.
use_icons_in_terminal() {
  local iconScript="${HOME}/.local/share/icons-in-terminal/icons_bash.sh"
  if [ -f "${iconScript}" ]; then
    . "${iconScript}"
  fi
}
use_icons_in_terminal

# Load forgit and other git-related things.
src_git_for_profile
# Load jenv and sdkman.
src_java_for_profile
# Load Ruby Version Manager (rvm).
src_ruby_for_profile
# Load Node.js Version Manager (nvm).
src_node_for_profile
# Load pyenv.
src_python_for_profile

# Print neofetch info when the terminal first opens
if [ -x "$(command -v neofetch)" ]; then
  neofetch 1>&2
fi

# One more src for good luck! Sometimes diraliases do not work after this
# script sources common.sh, so we add an extra src command here.
src

# vim:foldenable:foldmethod=marker:foldlevel=0
