#!/usr/bin/env bash

# Put the system to sleep
alias suspend="systemctl suspend"
alias afk="xscreensaver-command -lock"

# Pacman package manager {{{
# Install a package
alias pacs="sudo pacman -S"
# Search for a package in the Arch repositories
alias pacss="sudo pacman -Ss"
# Search for an installed package
alias pacq="sudo pacman -Q"
# Update/upgrade all installed packages
alias pacup="sudo pacman -Syyu"
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
