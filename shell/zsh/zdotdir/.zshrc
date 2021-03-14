#!/usr/bin/env zsh

# Make sure that file creation occurs with the normal permissions.
umask 022

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
DOTFILES_SHELL="${DOTFILES_HOME}/shell"
DOTFILES_ZSH="${DOTFILES_SHELL}/zsh"
DOTFILES_COMMON="${DOTFILES_SHELL}/common"
COMMON_SHELL_SOURCE="${DOTFILES_COMMON}/source"

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

# Source aliases/functions {{{
. "${COMMON_SHELL_SOURCE}/common.sh" && src_all
# }}}

# Plugins {{{
# Install zplug if it does not already exist
if [ ! -d "${ZPLUG_HOME}" ]; then
  git clone "https://github.com/zplug/zplug" "${ZPLUG_HOME}"
fi

if [ -f "${ZPLUG_HOME}/init.zsh" ]; then
  # Ready zplug
  . "${ZPLUG_HOME}/init.zsh"

  ## Register plugins

  # Show completion suggestions as you type, like in fish.
  zplug "zsh-users/zsh-autosuggestions"

  # real-time type-ahead autocompletion.
  # Fairly buggy. May want to wait before using.
  # zplug "marlonrichert/zsh-autocomplete"

  # Use extra community completions.
  zplug "zsh-users/zsh-completions"

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

# fzf {{{
FZF_SYSTEM="/usr/share/fzf"
FZF_USER_SHELL="${HOME}/.fzf/shell"

# Load fzf zsh keybindings
if [ -f "${FZF_SYSTEM}/key-bindings.zsh" ]; then
  . "${FZF_SYSTEM}/key-bindings.zsh"
elif [ -f "${FZF_USER_SHELL}/key-bindings.zsh" ]; then
  . "${FZF_USER_SHELL}/key-bindings.zsh"
fi

# Load fzf zsh completions
if [ -f "${FZF_SYSTEM}/completion.zsh" ]; then
  . "${FZF_SYSTEM}/completion.zsh"
elif [ -f "${FZF_USER_SHELL}/completion.zsh" ]; then
  . "${FZF_USER_SHELL}/completion.zsh"
fi

unset FZF_SYSTEM
unset FZF_USER_SHELL
# }}}

# Completions {{{
# Zsh has fantastic completion support by default, but we do have a few extra
# plugins we'd like to support. See functions_completion.sh for more details.
add_shell_completions
# }}}

# Keybindings {{{
# Ctrl-P to edit files with fzf or fzy.
# The '-s' option translates the input string to an output string that evaluates
# as a shell command, as opposed to evaluating the second option as a Zsh
# widget.
bindkey -s '^P' 'ctrlp^M'

# Ctrl-L to move cursor to end of autosuggestion.
bindkey '^L' autosuggest-accept

# Ctrl-O to open Ranger as a means of changing directories.
bindkey -s '^O' 'ranger-cd^M'
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

# Initialize the shell's auto-complete functionality.
# autoload -Uz compinit && compinit

# These icons are supplied by the icons-in-terminal project
use_icons_in_terminal() {
  local iconScript="${HOME}/.local/share/icons-in-terminal/icons_bash.sh"
  if [ -s "${iconScript}" ]; then
    . "${iconScript}"
  fi
}
use_icons_in_terminal

# One more src for good luck! Sometimes diraliases do not work after this
# script sources common.sh, so we add an extra src command here.
src

# Print neofetch info when the terminal first opens
if [ -x "$(command -v neofetch)" ]; then
  neofetch 1>&2
fi

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldenable:foldmethod=marker:foldlevel=0

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
