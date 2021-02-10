#!/usr/bin/env zsh

# Set variables {{{
# History
# The history file that lists previously run commands.
HISTFILE="${HISTFILE:-${HOME}/.zsh-history}"
# The number of commands to save
HISTSIZE=50000
# Trim history when its length exceeds SAVEHIST by 20%.
SAVEHIST=10000

# dotfiles repository files/directories
DOTFILES_HOME="${HOME}/.dotfiles"
DOTFILES_ZSH="${DOTFILES_HOME}/zsh"
DOTFILES_COMMON="${DOTFILES_HOME}/common"

# zplug is a fast, flexible plugin manager for zsh inspired by vimplug.
export ZPLUG_HOME="${HOME}/.zplug"
# }}}

# Options {{{
# Timestamp history and more
setopt extended_history
# Trim duplicated commands from history before trimming unique commands.
setopt hist_expire_dups_first
# If you run the same command multiple times in a row, only add it to history
# once.
setopt hist_ignore_dups
# Prefix a command with space to keep it out of history.
setopt hist_ignore_space
# Add commands to the history file as soon as they are run.
setopt inc_append_history
# Time stamp history and more.
setopt share_history

# Use the shorthand `/path/to/dir` for `cd /path/to/dir`
setopt auto_cd
# Keep a directory stack of all the directories you've visited in a session.
setopt autopushd
# Use `cd -n` to go back n directories in the stack.
setopt pushdminus
# Ignore duplicate entries in pushd directory stack.
setopt pushd_ignore_dups

# Move the cursor to the end of the word after accepting a completion.
setopt always_to_end
# TAB to show a menu of all completion suggestions.
# TAB a second time to enter the menu.
# TAB again to cycle through the list, or use the arrow keys.
# ENTER to accept a completion from the menu.
setopt auto_menu
# If you type TAB in the middle of a word, the cursor will move to the end
# of the word and zsh will open the completions menu.
setopt complete_in_word
# Disables the use of ^S to stop terminal output and the use of ^Q to resume it.
setopt flow_control
# Prevents the completion menu from showing, even if AUTO_MENU is set.
setopt menu_complete

# Adds support for command substitution.
setopt prompt_subst
# }}}

# Setup VI line editing mode {{{
# Currently unused in favor of zsh-vim-mode.
# bindkey -v
# }}}

# Plugins {{{
# Install zplug if it does not already exist
if [ ! -d "${ZPLUG_HOME}" ]; then
  git clone "https://github.com/zplug/zplug" "${ZPLUG_HOME}"
fi

if [ -f "${ZPLUG_HOME}/init.zsh" ]; then
  # Ready zplug
  source "${ZPLUG_HOME}/init.zsh"

  ## Register plugins

  # real-time type-ahead autocompletion.
  # zplug "marlonrichert/zsh-autocomplete"

  # autosuggestions {{{
  # Show completion suggestions as you type, like in fish.
  zplug "zsh-users/zsh-autosuggestions"
  # Add special keybindings
  # Use C-SPACE to move to end of current autosuggestion
  bindkey '^ ' autosuggest-accept
  # }}}

  # Fish-like syntax highlighting that colorizes your commands as you type them.
  zplug "zsh-users/zsh-syntax-highlighting"

  # Use C-n to leave insert mode.
  VIM_MODE_VICMD_KEY='^N'
  zplug "softmoth/zsh-vim-mode"

  # Install any registered plugins that have not yet been installed.
  if ! zplug check; then
    zplug install
  fi

  # Finally, load all plugins and add commands to $PATH.
  # (use '--verbose' flag to see output as each plugin loads).
  zplug load
fi
# }}}

# zstyle {{{
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

zstyle ':autocomplete:tab:*' insert-unambiguous yes
zstyle ':autocomplete:tab:*' fzf-completion yes
# }}}

# Prompt {{{
if [ -x "$(command -v starship)" ]; then
  # Use the amazing, cross-shell starship prompt if available.
  eval "$(starship init zsh)"
fi
# }}}

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldenable:foldmethod=marker:foldlevel=0
