#!/bin/sh

# Start terminator with a pre-defined layout
alias term="src && terminator -l coding"

# Convenience alias for locating errors in journalctl since the last boot.
alias jctl="journalctl -p 3 -xb"

# Shortcut to output a host of useful system information, including system
# hardware, CPU, drivers, Xorg, Desktop, Kernel, compiler versions, Processes,
# RAM usage, swap info, and much more.
alias sysinfo="inxi -eazy"

# Modeline for this file (LEAVE IT COMMENTED!!!)
# vim:foldenable:foldmethod=marker
