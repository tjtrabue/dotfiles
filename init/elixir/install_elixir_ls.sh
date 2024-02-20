#!/usr/bin/env bash

# Trap errors and print error message
set -uo pipefail
trap 's=$?; echo "$0: Error on line "${LINENO}": ${BASH_COMMAND}"; exit $s' ERR

# Variables {{{
declare DOTFILES_HOME="${HOME}/.dotfiles"
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"
declare WS="${WS:-${HOME}/workspace}"
declare INSTALL_DIR="${WS}/elixir-ls"
declare ELIXIR_LS_RELEASE_DIR="${INSTALL_DIR}/release"

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
${THIS_EXEC} | Install elixir-ls server

Once installed, and the release directory has been generated, make sure to add
the release directory to the \$PATH environment variable.

USAGE:
  ${THIS_EXEC} [OPTIONS]

OPTIONS:
  -h | --help
    Print the help message (this message) and exit.

  -i DIR | --install-dir DIR
   The directory where this script will clone the elixir-ls source code
   repository.

  -r DIR | --release-dir DIR
    The directory where this script will copy the elixir-ls release files and
    executables.
EOF
}

# Clone and build elixir-ls.
iels__install_elixir_ls() {
  local gitUrl="https://github.com/elixir-lsp/elixir-ls.git"

  clone_or_update_git_repo "${gitUrl}" "${INSTALL_DIR}"
  (
    log_info "Installing elixir-ls" &&
      cd "${INSTALL_DIR}" &&
      mix deps.get &&
      mix clean &&
      rm -rf "${INSTALL_DIR}/_build" "${ELIXIR_LS_RELEASE_DIR}" &&
      MIX_ENV=prod mix compile &&
      MIX_ENV=prod mix elixir_ls.release2 -o "${ELIXIR_LS_RELEASE_DIR}"
  )
}

# Add the release dir to PATH so that the executables are discoverable.
iels__add_elixir_ls_release_dir_to_path() {
  if [ -d "${ELIXIR_LS_RELEASE_DIR}" ]; then
    atp "${ELIXIR_LS_RELEASE_DIR}"
  else
    err "Could not find elixir-ls release dir at:" \
      "${BLUE}${ELIXIR_LS_RELEASE_DIR}${NC}"
    return 1
  fi
}

iels__main() {
  print_header "Installing elixir-ls"
  iels__install_elixir_ls
  iels__add_elixir_ls_release_dir_to_path
}
# }}}

# Parse CLI Options {{{
args=$(getopt -o hi:r: \
  --long help,install-dir:,release-dir: \
  -n 'install_elixir_ls.sh' \
  -- "$@")
eval set -- "$args"

# extract options and their arguments into variables.
while true; do
  case "$1" in
  -h | --help)
    _help
    exit 0
    ;;

  -i | --install-dir)
    case "$2" in
    "")
      shift 2
      ;;
    *)
      INSTALL_DIR="$2"
      ;;
    esac
    ;;

  -r | --release-dir)
    case "$2" in
    "")
      shift 2
      ;;
    *)
      ELIXIR_LS_RELEASE_DIR="$2"
      ;;
    esac
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

iels__main "${@}"

# vim:foldenable:foldmethod=marker:
