 # This file contains environment variables for use in the Z Shell.

 # Global File Variables {{{
 export DIR_ALIAS_FILE="$HOME/.zsh_dirs";
 export VARS_FILE="$HOME/.zshenv";
 export PATH_FILE="$HOME/.zsh_path";
 export MYVIMRC="$HOME/.vimrc";
 export MYZSHRC="$HOME/.zshrc";
 # }}}

 # Dotfiles Variables {{{
 export DOT="$HOME/.dotfiles";
 export VIM_HOME="$HOME/.vim";
 export DOT_ZSH="$DOT/zsh";
 export DOT_VENDORS="$DOT/vendors";
 export DOT_POWERLINE="$VENDORS/powerline";
 # }}}

# Editor {{{
export EDITOR='vim';
export USE_EDITOR=$EDITOR
export VISUAL=$EDITOR
# }}}

# Node {{{
# Enable persistent REPL history for `node`.
export NODE_REPL_HISTORY=~/.node_history;
# Allow 32³ entries; the default is 1000.
export NODE_REPL_HISTORY_SIZE='32768';
# Use sloppy mode by default, matching web browsers.
export NODE_REPL_MODE='sloppy';
# }}}

# Python {{{
# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
export PYTHONIOENCODING='UTF-8';
# }}}

# Java {{{
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home";
# }}}

# GlassFish {{{
export GLASSFISH_HOME="/usr/local/opt/glassfish/libexec";
# }}}

# Language {{{
 # Prefer US English and use UTF-8.
export LANG='en_US.UTF-8';
export LC_ALL='en_US.UTF-8';
# }}}

# Less Configuration {{{
# Get color support for 'less'
export LESS="-XR";

# Highlight section titles in manual pages.
export LESS_TERMCAP_mb="$(tput bold; tput setaf 33)"; # blue
export LESS_TERMCAP_md="$(tput bold; tput setaf 9)" # brick
export LESS_TERMCAP_me="$(tput sgr0)";
export LESS_TERMCAP_so="$(tput setaf 220)$(tput setab 5)"; # gold on magenta;
export LESS_TERMCAP_se="$(tput rmso; tput sgr0)";
export LESS_TERMCAP_us="$(tput smul; tput bold; tput setaf 230)"; # cream
export LESS_TERMCAP_ue="$(tput rmul; tput sgr0)";
export LESS_TERMCAP_mr="$(tput rev)";
export LESS_TERMCAP_mh="$(tput dim)";
export LESS_TERMCAP_ZN="$(tput ssubm)";
export LESS_TERMCAP_ZV="$(tput rsubm)";
export LESS_TERMCAP_ZO="$(tput ssupm)";
export LESS_TERMCAP_ZW="$(tput rsupm)";

# Don’t clear the screen after quitting a manual page.
export MANPAGER='less -X';
# }}}

# Terminal {{{
# Make sure terminal supports 256 colors.
export TERM="screen-256color";

# Make sure that exiting insert mode while using zle in vi mode goes by fast
export KEYTIMEOUT=1;
# }}}

# Colors {{{
# Highlight dirs
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/local/share/zsh-syntax-highlighting/highlighters

# Color Codes
export BOLD="$(tput bold)";
export NC="$(tput sgr0)";
export BLACK="$(tput setaf 0)";
export RED="$(tput setaf 1)";
export GREEN="$(tput setaf 2)";
export YELLOW="$(tput setaf 3)";
export BLUE="$(tput setaf 4)";
export MAGENTA="$(tput setaf 5)";
export CYAN="$(tput setaf 6)";
export WHITE="$(tput setaf 7)";
export ORANGE="$(tput setaf 9)";
export BOLD_RED="$(tput bold; tput setaf 1)";
export VIOLET="$(tput bold; tput setaf 5)";
# }}}

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldmethod=marker
