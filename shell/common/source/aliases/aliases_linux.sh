#!/bin/sh

# Start terminator with a pre-defined layout
alias term="src && terminator -l coding"

# Convenience alias for locating errors in journalctl since the last boot.
alias jctl="journalctl -p 3 -xb"

# Modeline for this file (LEAVE IT COMMENTED!!!)
# vim:foldenable:foldmethod=marker
