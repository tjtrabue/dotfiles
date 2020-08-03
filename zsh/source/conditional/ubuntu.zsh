#!/usr/bin/env zsh

# This file contains aliases and functions specific to the Ubuntu GNU/Linux distro.
# Exit if not Ubuntu
is_ubuntu || return 1;

# Package management
alias update="sudo apt-get -qq update && sudo apt-get upgrade";
alias install="sudo apt-get install";
alias remove="sudo apt-get remove";
alias search="apt-cache search";

# Make 'less' more
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)";