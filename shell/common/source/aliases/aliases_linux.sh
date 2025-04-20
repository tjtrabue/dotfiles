#!/bin/sh

# Convenience alias for locating errors in journalctl since the last boot.
alias jctl="journalctl -p 3 -xb"

# Reload Systemd configuration
alias resysd="sudo systemctl daemon-reload"

# Lock the X session using XScreenSaver.
# This requires xscreensaver to be installed and the xscreensaver daemon to be
# running (by issuing `xscreensaver &`).
alias sslock="xscreensaver-command -lock"

# Shortcut to output a host of useful system information, including system
# hardware, CPU, drivers, Xorg, Desktop, Kernel, compiler versions, Processes,
# RAM usage, swap info, and much more.
alias sysinfo="inxi -eazy"

# Modeline for this file (LEAVE IT COMMENTED!!!)
# vim:foldenable:foldmethod=marker
