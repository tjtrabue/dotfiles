#!/usr/bin/env bash

# Trap errors and print error message
set -uo pipefail
trap 's=$?; echo "$0: Error on line "${LINENO}": ${BASH_COMMAND}"; exit $s' ERR

# Variables {{{
declare DOTFILES_HOME="${HOME}/.dotfiles"
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"

# Logging variables
declare LOG_TO_FILE="${LOG_TO_FILE:-''}"
declare LOG_LEVEL="${LOG_LEVEL:-3}"
# }}}

# Imports {{{
. "${DOTFILES_HOME}/shell/common/source/common.sh" && src
# }}}

# Functions {{{
_help() {
  command cat <<EOF
${THIS_EXEC} | <insert description here>

USAGE:
  ${THIS_EXEC} [OPTIONS]

OPTIONS:
  -h | --help
    Print the help message (this message) and exit.

  -q | --quiet
    Only print error messages.

  -v | --verbose
    Increase the quantity of debugging output. Useful for debugging. This option
    may be presented multiple times to further increase logging verbosity.
EOF
}

main() {
  print_header "Hello, world!"
}
# }}}

# Parse CLI Options {{{
args=$(getopt -o hqv \
  --long help,quiet,verbose \
  -n '<name_here>' \
  -- "$@")
eval set -- "$args"

# extract options and their arguments into variables.
while true; do
  case "$1" in
  -h | --help)
    _help
    exit 0
    ;;

  -q | --quiet)
    LOG_LEVEL=1
    shift
    ;;

  -v | --verbose)
    ((LOG_LEVEL += 1))
    shift
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
