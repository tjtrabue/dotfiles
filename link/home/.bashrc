#!/usr/bin/env bash

# Make sure that file creation occurs with the normal permissions.
umask 022

# PATH {{{
# IMPORTANT: This PATH may be overriden by the spath function. This only exists
# in order to make sure that certain necessary executables are found before
# running the main shell profile logic.
export PATH="/Users/tom.trabue/perl5/bin:/Users/tom.trabue/go/bin:/Users/tom.trabue/.asdf/bin:/Users/tom.trabue/.roswell/bin:/Users/tom.trabue/.rvm/bin:/Users/tom.trabue/.pyenv/shims:/Users/tom.trabue/.pyenv/bin:/Users/tom.trabue/.sdkman/candidates/maven/current/bin:/Users/tom.trabue/.sdkman/candidates/kotlin/current/bin:/Users/tom.trabue/.sdkman/candidates/java/current/bin:/Users/tom.trabue/.sdkman/candidates/gradle/current/bin:/Users/tom.trabue/.jenv/bin:/Users/tom.trabue/.cargo/bin:/Users/tom.trabue/.npm-global/bin:/Users/tom.trabue/.luarocks/bin:/Users/tom.trabue/.cabal/bin:/Users/tom.trabue/.ghcup/bin:/Users/tom.trabue/.fzf/bin:/Users/tom.trabue/.dotfiles/bin:/Users/tom.trabue/.local/bin:/Users/tom.trabue/bin:/usr/local/Cellar/libtool/2.4.6_4/libexec/gnubin:/usr/local/Cellar/grep/3.7/libexec/gnubin:/usr/local/Cellar/gnu-which/2.21/libexec/gnubin:/usr/local/Cellar/gnu-tar/1.34/libexec/gnubin:/usr/local/Cellar/gnu-sed/4.8/libexec/gnubin:/usr/local/Cellar/gnu-indent/2.2.12_1/libexec/gnubin:/usr/local/Cellar/gawk/5.1.1/libexec/gnubin:/usr/local/Cellar/findutils/4.8.0_1/libexec/gnubin:/usr/local/Cellar/ed/1.17/libexec/gnubin:/usr/local/Cellar/coreutils/9.0/libexec/gnubin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/bin:/sbin:/bin"
# }}}

# Where the magic happens:
DOTFILES_HOME="${HOME}/.dotfiles"
DOTFILES_COMMON="${DOTFILES_HOME}/shell/common"

# Pull in common shell aliases/functions as soon as possible.
. "${DOTFILES_COMMON}/source/common.sh"

# Activate custom aliases/functions
src_all

# Shell completion {{{
# Include extra CLI completions if available.
add_shell_completions
# }}}

# Make sure to set prompt to something simple for Emacs TRAMP {{{
if [ "$TERM" == "dumb" ]; then
  PS1="tramp $ "
fi
# }}}

# Load fzf keybindings {{{
if [ -f "${HOME}/.fzf.bash" ]; then
  . "${HOME}/.fzf.bash"
fi
# }}}

# One more src for good luck!
src

# These icons are supplied by the icons-in-terminal project.
use_icons_in_terminal() {
  local iconScript="${HOME}/.local/share/icons-in-terminal/icons_bash.sh"
  if [ -f "${iconScript}" ]; then
    . "${iconScript}"
  fi
}
use_icons_in_terminal

# Print system info when the terminal first opens.
show_system_info

# vim:foldenable:foldmethod=marker:foldlevel=0
