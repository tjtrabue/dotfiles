#!/bin/sh

# Functions related to GNU Parallel.

# Export the shell environment to GNU Parallel. This allows using shell aliases
# and functions as the primary command for parallel.
src_parallel_for_profile() {
  if [ -x "$(command -v env_parallel)" ]; then
    __source_parallel_env
  fi
}

__source_parallel_env() {
  local currShell="$(currentshell)"

  case "${currShell}" in
    "bash")
      log_info "Exporting Bash environment for GNU Parallel"
      . "$(which env_parallel.bash)"
      ;;
#    "fish")
#      log_info "Exporting Fish environment for GNU Parallel"
#      . (which env_parallel.fish)
#      ;;
    "zsh")
      log_info "Exporting Zsh environment for GNU Parallel"
      . "$(which env_parallel.zsh)"
      ;;
    *)
      err "Unknown shell for Parallel: ${MAGENTA}${currShell}${NC}"
      return 1
      ;;
  esac
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
