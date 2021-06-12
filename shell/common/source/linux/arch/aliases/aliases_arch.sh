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

### Special AUR helper aliases:
# AUR Build (bulid and install package in current directory)
alias aurhb="\${AUR_HELPER} \${AUR_HELPER_BUILD_CMD}"
# AUR Clean
alias aurhc="\${AUR_HELPER} \${AUR_HELPER_CLEAN_CMD}"
# AUR Get
alias aurhg="\${AUR_HELPER} \${AUR_HELPER_GET_CMD}"
# AUR Install
alias aurhi="\${AUR_HELPER} \${AUR_HELPER_INSTALL_CMD}"
alias aurhinc="\${AUR_HELPER} \${AUR_HELPER_INSTALL_CMD} \
  \${AUR_HELPER_NO_CONFIRM_FLAG}"
# AUR Query
alias aurhq="\${AUR_HELPER} \${AUR_HELPER_QUERY_CMD}"
# AUR Remove
alias aurhr="\${AUR_HELPER} \${AUR_HELPER_REMOVE_CMD}"
# AUR Search
alias aurhs="\${AUR_HELPER} \${AUR_HELPER_SEARCH_CMD}"
# AUR Update
alias aurhu="\${AUR_HELPER} \${AUR_HELPER_UPDATE_CMD}"
# }}}

# vim:foldenable:foldmethod=marker:
