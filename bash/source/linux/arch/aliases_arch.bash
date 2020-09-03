#!/usr/bin/env bash

# Put the system to sleep
alias suspend="systemctl suspend"
alias afk="xscreensaver-command -lock"

# Pacman package manager
alias pacq="sudo pacman -Q"
alias pacs="sudo pacman -S"
alias pacss="sudo pacman -Ss"
alias pacsyu="sudo pacman -Syyu"

# Aura AUR package manager
alias aurai="sudo aura -Aax"
alias aurah="sudo aura -Aax --hotedit"
alias aurau="sudo aura -Aaxu --noconfirm"

# vim:foldenable:foldmethod=marker:
