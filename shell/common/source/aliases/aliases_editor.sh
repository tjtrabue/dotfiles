#!/bin/sh

# Basic aliases {{{
# Set an alias that invokes the default editor program.
alias edit="\${EDITOR:-vim}"
# Shorthand for 'edit'.
alias e="edit"
# Get around that muscle memory!
alias vim="edit"
# Fix that common typo.
alias ivm="vim"

# Edit a file (with the configured $EDITOR) as the super user
alias se="sudoedit"
# }}}

# Home file editing aliases {{{
alias vbpr="edit \${HOME}/.bash_profile"
alias vbrc="edit \${HOME}/.bashrc"
alias virc="edit \${HOME}/.inputrc"
alias vvrc="edit \${HOME}/.vimrc"
alias verc="edit \${HOME}/.emacs"
alias vdirs="edit \${DIR_ALIAS_FILE}"
alias vdhist="edit \${DIR_HIST_FILE}"
alias vvars="edit \${HOME}/.vars"
alias vpath="edit \${HOME}/.path"
alias vpr="edit \${HOME}/.profile"
alias vxpr="edit \${HOME}/.xprofile"
alias vxre="edit \${HOME}/.Xresources"
alias vgconf="edit \${HOME}/.gitconfig"
alias vtconf="edit \${HOME}/.tmux.conf"
alias vzrc="edit \${ZDOTDIR}/.zshrc"
# }}}

# ~/.config files editing aliases {{{
alias vi3="edit \${USER_CONF}/i3/config"
# }}}

# vim:foldmethod=marker:
