#!/usr/bin/env bash

# Basic aliases {{{
alias edit="$EDITOR"
alias e="edit"
# Get around that muscle memory!
alias vim="edit"

# Edit a file (with the configured $EDITOR) as the super user
alias se="sudoedit"
# }}}

# Source file editiing aliases {{{
alias vau="edit $DOTFILES_HOME/bash/source/aliases_universal.bash"
alias vag="edit $DOTFILES_HOME/bash/source/aliases_git.bash"
alias vae="edit $DOTFILES_HOME/bash/source/aliases_editor.bash"
alias vfu="edit $DOTFILES_HOME/bash/source/functions_universal.bash"
alias vfg="edit $DOTFILES_HOME/bash/source/functions_git.bash"
alias vcolors="edit $DOTFILES_HOME/bash/source/colors.bash"
# }}}

# Home file editing aliases {{{
alias vbpr="edit $HOME/.bash_profile"
alias vbrc="edit $HOME/.bashrc"
alias virc="edit $HOME/.inputrc"
alias vvrc="edit $HOME/.vimrc"
alias verc="edit $HOME/.emacs"
alias vdirs="edit $HOME/.dirs"
alias vvars="edit $HOME/.vars"
alias vpath="edit $HOME/.path"
alias vpr="edit $HOME/.profile"
alias vxre="edit $HOME/.Xresources"
alias vgconf="edit $HOME/.gitconfig"
alias vtconf="edit $HOME/.tmux.conf"
# }}}

# ~/.config files editing aliases {{{
USER_CONF="$HOME/.config"
alias vi3="edit $USER_CONF/i3/config"
# }}}

# vim:foldmethod=marker:
