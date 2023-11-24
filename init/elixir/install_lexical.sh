#!/usr/bin/env bash

# Trap errors and print error message
set -uo pipefail
trap 's=$?; echo "$0: Error on line "${LINENO}": ${BASH_COMMAND}"; exit $s' ERR

# Variables {{{
declare DOTFILES_HOME="${HOME}/.dotfiles"
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"
declare WS="${WS:-${HOME}/workspace}"
declare LEXICAL_INSTALL_DIR="${WS}/lexical"
declare LEXICAL_BIN_DIR="${LEXICAL_INSTALL_DIR}/_build/dev/package/lexical/bin"

# Logging variables
declare LOG_TO_FILE=""
declare LOG_LEVEL=3
# }}}

# Imports {{{
. "${DOTFILES_HOME}/shell/common/source/common.sh" && src
# }}}

# Functions {{{
_help() {
  command cat <<EOF
${THIS_EXEC} | Install the Lexical LSP server for Elixir

Once this script completes, if we assume the full path to the Lexical repo is
"/home/user/projects/lexical", the Lexical executables can be found at:
"/home/user/projects/lexical/_build/dev/package/lexical/bin". You should
add this directory to your PATH variable once this script completes. Otherwise,
some LSP clients (such as lsp-bridge for Emacs) may not be able to start or
interact properly with Lexical.

USAGE:
  ${THIS_EXEC} [OPTIONS]

OPTIONS:
  -h | --help
    Print the help message (this message) and exit.
EOF
}

install_lexical() {
  local gitUrl="git@github.com:lexical-lsp/lexical.git"

  clone_or_update_git_repo "${gitUrl}" "${LEXICAL_INSTALL_DIR}"
  (
    log_info "Installing Lexical" &&
      cd "${LEXICAL_INSTALL_DIR}" &&
      mix deps.get &&
      INDEXING_ENABLED=true mix package
  )
}

add_lexical_bin_dir_to_path() {
  atp "${LEXICAL_BIN_DIR}"
}

main() {
  print_header "Installing Lexical (LSP Server for Elixir)"
  install_lexical
  add_lexical_bin_dir_to_path
}
# }}}

# Parse CLI Options {{{
args=$(getopt -o h --long help -n 'install_lexical.sh' -- "$@")
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
