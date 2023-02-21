#!/bin/sh

# Start Emacs with a maximized initial frame.
alias emacs="command emacs -mm"

# Start Emacs in servre mode, which you can later connect to using
# `emacsclient'
alias emd="command emacs --daemon"

# Kill a running emacs daemon.
alias killemd="command emacsclient -e '(kill-emacs)'"

# Connect to a running Emacs server.
alias emc="command emacsclient"

# vim:foldenable:foldmethod=marker:foldlevel=0