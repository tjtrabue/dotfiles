#!/usr/bin/env bash

# Put the system to sleep
alias suspend="systemctl suspend"
alias afk="xscreensaver-command -lock"

# Listing kernel drivers {{{
alias lskern="sudo lspci -k | less"
# }}}

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

# AUR helper {{{
# 'aurh' is the alias for the user's selected AUR helper program
alias aurh="${AUR_HELPER:-paru}"
alias aurhi="\${AUR_HELPER} \${AUR_HELPER_INSTALL_CMD}"
alias aurhinc="\${AUR_HELPER} \${AUR_HELPER_INSTALL_CMD} \
  \${AUR_HELPER_NO_CONFIRM_FLAG}"
alias aurhu="\${AUR_HELPER} \${AUR_HELPER_UPDATE_CMD}"
alias aurhr="\${AUR_HELPER} \${AUR_HELPER_REMOVE_CMD}"
alias aurhsearch="\${AUR_HELPER} \${AUR_HELPER_SEARCH_CMD}"
alias aurhclean="\${AUR_HELPER} \${AUR_HELPER_CLEAN_CMD}"
# }}}

# vim:foldenable:foldmethod=marker:
