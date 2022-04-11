#!/bin/sh

# A beautiful function that provides fuzzy search wrapping around many
# frequently-used Git commands.
fuzzygit() {
  local toplevelCmd="${1}"
  local fuzzyFinderCmd

  fuzzyFinderCmd="$(__get_fuzzy_finder_command)"

  if [ -z "${toplevelCmd}" ]; then
    toplevelCmd="$(__fuzzygit_get_toplevel_command "${fuzzyFinderCmd}")"
  fi

  case "${toplevelCmd}" in
  "add")
    __fuzzygit_add "${fuzzyFinderCmd}"
    ;;
  *)
    err "Unknown fuzzygit toplevel command: ${YELLOW}${toplevelCmd}${NC}"
    return 1
    ;;
  esac
}

############################################
##                 Toplevel               ##
############################################

__fuzzygit_get_toplevel_command() {
  local fuzzyFinderCmd="${1}"
  local toplevelCmds="$(__fuzzygit_get_toplevel_commands)"
  eval "$(__fuzzygit_construct_fuzzy_find_cmd_line \
    "${fuzzyFinderCmd}" \
    "printf '${toplevelCmds}' | tr ' ' '\n'")"
}

__fuzzygit_get_toplevel_commands() {
  local toplevelCmds="add stash"
  printf "%s" "${toplevelCmds}"
}

############################################
##                   Add                  ##
############################################

__fuzzygit_add() {
  __fuzzygit_run_git_cmd "${fuzzyFinderCmd}" "git ls-files -m" "git add"
}

############################################
##                  Stash                 ##
############################################

__fuzzygit_get_stash_subcommands() {
  local stashSubcommands="clear pop"
  printf "%s" "${stashSubcommands}"
}

############################################
##              Arbitrary Git             ##
############################################

# For a given fuzzy find commandline tool (1), list Git objects (2), and run
# a Git command on the selected subset of those objects (3).
__fuzzygit_run_git_cmd() {
  local fuzzyFinderCmd="${1}"
  local gitListingCmd="${2}"
  local primaryGitCmd="${3}"
  local fuzzyFindCommandLine="$(__fuzzygit_construct_fuzzy_find_cmd_line \
    "${fuzzyFinderCmd}" "${gitListingCmd}")"
  local selectedObjects="$(eval "${fuzzyFindCommandLine}")"

  if [ -n "${selectedObjects}" ]; then
    eval "${primaryGitCmd} ${selectedObjects}"
  fi
}

############################################
##                  Misc                  ##
############################################

# Construct a fuzzy find command from the specified fuzzy finder executable and
# an arbitrary shell string representing some type of listing command.
__fuzzygit_construct_fuzzy_find_cmd_line() {
  local fuzzyFinderCmd="${1}"
  local fuzzygitCmd="${2}"
  local finalCmd=""

  case "${fuzzyFinderCmd}" in
  "fzf")
    finalCmd="${fuzzygitCmd} | ${fuzzyFinderCmd}"
    ;;
  "fzy")
    finalCmd="${fuzzygitCmd} | ${fuzzyFinderCmd}"
    ;;
  esac

  printf "%s" "${finalCmd}"
}

__get_fuzzy_finder_command() {
  local fuzzyFinderCmd="fzf"

  if [ -x "$(command -v fzf)" ]; then
    fuzzyFinderCmd="fzf"
  elif [ -x "$(command -v fzy)" ]; then
    fuzzyFinderCmd="fzy"
  fi

  printf "%s" "${fuzzyFinderCmd}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1