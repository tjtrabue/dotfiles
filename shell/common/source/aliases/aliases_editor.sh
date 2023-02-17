#!/bin/sh

# Basic aliases {{{
# Set an alias that invokes the default editor program.
alias edit="\${EDITOR:-\$(command vim)}"
# Shorthand for 'edit'.
alias e="edit"
# Get around that muscle memory!
alias vim="edit"
# Fix that common typo.
alias ivm="edit"

# Edit a file (with the configured $EDITOR) as the super user
alias se="sudoedit"
# }}}

# Home file editing aliases {{{
alias vbpr="edit \${HOME}/.bash_profile"
alias vbrc="edit \${HOME}/.bashrc"
alias virc="edit \${HOME}/.inputrc"
alias vvrc="editvimrc"
alias verc="edit \${HOME}/.emacs"
alias vdirs="edit \${DIR_ALIAS_FILE}"
alias vdhist="edit \${DIR_HIST_FILE}"
alias vvars="edit \${HOME}/.vars"
alias vvaro="edit \${HOME}/.var_overrides"
alias vpath="edit \${HOME}/.path"
alias vspath="edit \${HOME}/.path_static"
alias vpr="edit \${HOME}/.profile"
alias vxpr="edit \${HOME}/.xprofile"
alias vxre="edit \${HOME}/.Xresources"
alias vgconf="edit \${USER_CONF}/git/config" # user global git config
alias vgconfl="edit \${HOME}/.gitconfig"     # machine-local git config
alias vtconf="edit \${HOME}/.tmux.conf"
alias vzenv="edit \${HOME}/.zshenv"
alias vzrc="edit \${ZDOTDIR}/.zshrc"
# }}}

# ~/.config files editing aliases {{{
alias vi3="edit \${USER_CONF}/i3/config"
# }}}

# Emacs {{{

# Start emacs with a maximized initial frame.
alias emacs="emacs -mm"
# }}}

# vim:foldmethod=marker
