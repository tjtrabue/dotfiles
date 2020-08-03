#!/usr/bin/env zsh
# Author: Tom Trabue
# Date: 3/23/20127

# Keybindings {{{
# }}}

# ZLE Widgets {{{
# }}}

# Options {{{
# }}}

# $PATH {{{
# Add GNU coreutils to $PATH first thing (if they exist) so that there
# is no need to prefix their names with `g' when invoking them.
# Also add their entries to the $MANPATH so that their manual entries
# may be accessed with their default names.
if [[ $(uname) -eq "Dawin" ]] && $(brew info coreutils &> /dev/null); then
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH";
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH";
fi
export PATH=$HOME/bin:/usr/local/bin:$PATH
# }}}

# Functions {{{
# OS detection
is_ubuntu() {
    [[ "$(cat /etc/issue 2> /dev/null)" =~ Ubuntu ]] || return 1;
}

is_ubuntu_desktop() {
    dpkg -l ubuntu-desktop > /dev/null 2>&1 || return 1;
}

is_osx() {
    [[ "$OSTYPE" =~ ^darwin ]] || return 1;
}

get_os() {
    local os;
    for os in osx ubuntu ubuntu_desktop; do
        is_${os}; [[ $? == ${1:-0} ]] && echo $os;
    done
}

# Source extra environment files:
src() {
    local zshSource="$DOT/zsh/source";
    local file;
    local osType="$(get_os)";
    for file in $(find "$zshSource" -maxdepth 1 -mindepth 1 -type f); do
        [[ -f "$file" ]] && source "$file";
    done
    for file in "$HOME"/.{path,zsh_dirs,zshenv}; do
        [[ -f "$file" ]] && source "$file";
    done

    case "$osType" in
        osx)
            source "$zshSource/conditional/osx.zsh";
            ;;
        ubuntu)
            source "$zshSource/conditoinal/ubuntu.zsh";
            ;;
        *)
            echo "ERROR: Line $LINENO - Unknown OS type $osType";
            return 1;
            ;;
    esac
}
src;
# }}}

# Solarized {{{
# Use solarized dircolors
# ~/.dir_colors is symlinked to dircolors.256dark in the seebi/dircolors-solarized project
eval `dircolors ~/.dir_colors`
# }}}

# oh-my-zsh {{{
# Path to dotfiles installation.
export DOT="${HOME}/.dotfiles";

# Path to your oh-my-zsh installation.
export ZSH=$DOT/vendors/oh-my-zsh;

# Take advantage of Awesome Terminal Fonts glyphs.
# NOTE: This line MUST come before the specification of the powerlevel9k theme!
POWERLEVEL9K_MODE='awesome-fontconfig'

# Uncomment this line if want to use Awesome Terminal Fonts with a patched font
# (Instead of changing the system's fallback font plist).
#POWERLEVEL9K_MODE='awesome-patched'

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="powerlevel9k/powerlevel9k"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git vi-mode zsh-completions zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# }}}

# Powerlevel9k {{{
# Custom settings for Powerlevel9k theme:
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(ssh context dir vi_mode rbenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs time ip)

# Output time, date, and a symbol from the "Awesome Powerline Font" set
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M:%S \uE868  %d.%m.%y}"

# Add custom function to take care of vi-mode integration that was removed from main
# powerlevel9k repo due to performance issues.
# TODO: Remove this snippet when powerlevel9k is updated with fixed vi-mode integration.
#function zle-line-init {
#  powerlevel9k_prepare_prompts
#  if (( ${+terminfo[smkx]} )); then
#    printf '%s' ${terminfo[smkx]}
#  fi
#  zle reset-prompt
#  zle -R
#}
#
#function zle-line-finish {
#  powerlevel9k_prepare_prompts
#  if (( ${+terminfo[rmkx]} )); then
#    printf '%s' ${terminfo[rmkx]}
#  fi
#  zle reset-prompt
#  zle -R
#}
#
#function zle-keymap-select {
#  powerlevel9k_prepare_prompts
#  zle reset-prompt
#  zle -R
#}
#
#zle -N zle-line-init
#zle -N ale-line-finish
#zle -N zle-keymap-select
# END vi-mode fix snippet.
# }}}

# Make sure to set prompt to something simple for Emacs TRAMP {{{
if [ "$TERM" == "dumb" ]; then
  unsetopt zle
  PS1="tramp $ "
fi
# }}}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/tjtrabue/.sdkman"
[[ -s "/home/tjtrabue/.sdkman/bin/sdkman-init.sh" ]] && source "/home/tjtrabue/.sdkman/bin/sdkman-init.sh"

# Modeline for this file (LEAVE IT COMMENTED!)
# vim:foldmethod=marker
