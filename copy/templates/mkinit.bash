#!/usr/bin/env bash

# Trap errors and print error message
set -Eeuo pipefail
trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

# Variables {{{
declare DOTFILES_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"

# Logging variables
declare LOG_TO_FILE=""
declare LOG_LEVEL=3

declare IMPORT="${DOTFILES_REPO}/bash/source"
# }}}

# Imports {{{
. "${IMPORT}/colors.bash"
. "${IMPORT}/functions_log.bash"
# }}}

# Functions {{{
_help() {
  cat <<EOF
${THIS_EXEC}

<insert description here>

USAGE:
  ${THIS_EXEC} [OPTIONS]

OPTIONS:
  -h | --help
    Print the help message (this message) and exit.
EOF
}

main() {
  log_info "Hello, world!"
}
# }}}

# Parse CLI Options {{{
args=$(getopt -o h --long help-n 'init_<name_here>' -- "$@")
eval set -- "$args"

# extract options and their arguments into variables.
while true; do
  case "$1" in
  -h | --help)
    _help
    exit 0
    ;;

  --)
    shift
    break
    ;;

  *)
    err "Unknown option $1 to ${THIS_EXEC}"
    exit 2
    ;;
  esac
done
# }}}

main "${@}"

# vim:foldenable:foldmethod=marker:
