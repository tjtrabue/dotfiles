#!/bin/sh

# Read a path file's contents into a path variable, then export the path
# variable.
# @param [$1=~/.path] - The path file to read from
# @param [$2=PATH]    - The path variable to export
spath() {
  local pathFile="${1:-${PATH_FILE}}"
  local pathVar="${2:-PATH}"
  local constructedPath=""
  constructedPath="$(__evaluate_paths "${pathFile}" | tr '\n' ':' |
    sed 's/:$//')"

  eval "${pathVar}=${constructedPath}"
  export "${pathVar?}"
}

# Print paths in $PATH file with all environment variables evaluated
__evaluate_paths() {
  local pathFile="${1:-${PATH_FILE}}"
  # Make sure that paths are evaluated in reverse order from their listing in
  # the .path file since we want more recently added paths to take precedence
  # over older ones.
  # The awk command at the end removes cuplicates from the listing.
  eval echo "$(xargs -a "${pathFile}")" | tr ' ' '\n' | tac | awk '!x[$0]++'
}

# vim:foldenable:foldmethod=marker:foldlevel=0
