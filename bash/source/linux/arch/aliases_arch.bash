#!/usr/bin/env bash

# Put the system to sleep
alias suspend="systemctl suspend"
alias afk="xscreensaver-command -lock"

# Pacman package manager {{{
alias pacq="sudo pacman -Q"
alias pacs="sudo pacman -S"
alias pacss="sudo pacman -Ss"
alias pacsyu="sudo pacman -Syyu"
# }}}

# Aura AUR package manager {{{
# Install an AUR package
alias aurai="sudo aura -Aax"
# Install an AUR package, allowing the PKGBUILD file to be edited before
# installing.
alias aurah="sudo aura -Aax --hotedit"
# Upgrade all install AUR packages
alias aurau="sudo aura -Aaxu --noconfirm"
# }}}

# vim:foldenable:foldmethod=marker:
