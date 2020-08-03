#!/usr/bin/env bash

# Trap errors and print error message
set -uo pipefail
trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

#  Setup / Cleanup {{{
# Performs initial setup.
function setup() {
  log_info "Setting up...";
}

# Cleanup after the program finishes.
function cleanup() {
  log_info "Cleaning up...";
}
trap cleanup EXIT
# }}}

# Printing Functions {{{
# General purpose printing function
function echoe() {
  echo -e "$*" 1>&2;
}

# Error printing function.
function err() {
  local funcName="${FUNCNAME[1]}";
  local lineNo="${BASH_LINENO[1]}";
  echoe "[${RED}ERROR${NC}:${MAGENTA}${funcName}${NC}:${CYAN}${lineNo}${NC}] $*";
}

# Warning printing function.
function warn() {
  local funcName="${FUNCNAME[1]}";
  local lineNo="${BASH_LINENO[1]}";
  echoe "[${YELLOW}WARNING${NC}:${MAGENTA}${funcName}${NC}:${CYAN}${lineNo}${NC}] $*";
}

# Success printing function.
function succ() {
  local funcName="${FUNCNAME[1]}";
  local lineNo="${BASH_LINENO[1]}";
  echoe "[${GREEN}SUCCESS${NC}:${MAGENTA}${funcName}${NC}:${CYAN}${lineNo}${NC}] $*";
}

# Success printing function.
function log_info() {
  local funcName="${FUNCNAME[1]}";
  local lineNo="${BASH_LINENO[1]}";
  echoe "[${GREEN}INFO${NC}:${MAGENTA}${funcName}${NC}:${CYAN}${lineNo}${NC}] $*";
}
# }}}

# Primary Functions {{{
function main() {
  setup;
}
# }}}

# Variable Definitions {{{
# Directories
declare DOTFILES_HOME="$HOME/.dotfiles";
declare DOTFILES_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)";
declare DOTFILES_LINK="$DOTFILES_REPO/link";
declare LINK_HOME="$DOTFILES_LINK/home";
declare LINK_CONFIG="$DOTFILES_LINK/config";
declare BACKUP_DIR="$DOTFILES_REPO/backup";
declare -a BACKUP_FILES=($(find "$LINK_HOME" -type f -exec basename '{}' \;));

# Colors {{{
# Foreground:
declare NC="\e[39m"; # Default color
declare BLACK="\e[30m";
declare RED="\e[31m";
declare GREEN="\e[32m";
declare YELLOW="\e[33m";
declare BLUE="\e[34m";
declare MAGENTA="\e[35m";
declare CYAN="\e[36m";
declare LIGHT_GRAY="\e[37m";
declare DARK_GRAY="\e[90m";
declare LIGHT_RED="\e[91m";
declare LIGHT_GREEN="\e[92m";
declare LIGHT_YELLOW="\e[93m";
declare LIGHT_BLUE="\e[94m";
declare LIGHT_MAGENTA="\e[95m";
declare LIGHT_CYAN="\e[96m";
declare WHITE="\e[97m";

# Background:
declare NBG="\e[49m"; # Default background
declare BLACK_BG="\e[40m";
declare RED_BG="\e[41m";
declare GREEN_BG="\e[42m";
declare YELLOW_BG="\e[43m";
declare BLUE_BG="\e[44m";
declare MAGENTA_BG="\e[45m";
declare CYAN_BG="\e[46m";
declare LIGHT_GRAY_BG="\e[47m";
declare DARK_GRAY_BG="\e[100m";
declare LIGHT_RED_BG="\e[101m";
declare LIGHT_GREEN_BG="\e[102m";
declare LIGHT_YELLOW_BG="\e[103m";
declare LIGHT_BLUE_BG="\e[104m";
declare LIGHT_MAGENTA_BG="\e[105m";
declare LIGHT_CYAN_BG="\e[106m";
declare WHITE_BG="\e[107m";
# }}}
# }}}

main;

# vim:foldenable:foldmethod=marker:
