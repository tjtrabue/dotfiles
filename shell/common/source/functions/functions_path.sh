#!/bin/sh

# Adds a path to the $PATH environment variable.
atp() {
  local pathToAdd="${1:-$(pwd)}"
  pathToAdd="$(shortpath "${pathToAdd}")"

  if ! __evaluate_paths | grep -Fxq "$(eval echo "${pathToAdd}")"; then
    echo "${pathToAdd}" >>"${PATH_FILE}"
  fi
}

# Prefix a given path with any existing aliases
shortpath() {
  local dir_alias=""
  local best_var=""
  local to_replace=""
  local best_to_replace=""
  local input_path="$1"

  grep -E "^\s*export" "${DIR_ALIAS_FILE}" | sed 's:^ *export *::' | sed 's:=.*::' | {
    while read -rs var; do
      dir_alias="$(env | grep -E "^${var}\b")"
      if [ -n "${dir_alias}" ]; then
        to_replace="$(echo "${dir_alias}" | sed -e "s:${var}=::" -e 's:"::g')"
        if [[ "$input_path" =~ ^"${to_replace%/}/".* ]]; then
          if [ "${#to_replace}" -gt "${#best_to_replace}" ]; then
            best_to_replace="${to_replace}"
            best_var="${var}"
          fi
        fi
      fi
    done
    if [ -n "${best_to_replace}" ]; then
      input_path=${input_path//${best_to_replace}/\$${best_var}}
    fi
    echo "${input_path}"
  }
}

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

# echo the $PATH environment variable to the terminal.
epath() {
  printf "%s\n" "${PATH}"
}

# Source the LuaRocks module path.
src_lua_path() {
  if [ "$(command -v luarocks)" != "" ]; then
    eval "$(luarocks path)"
  fi
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

# Write additional paths to the ~/.path file depending on the operating system.
add_extra_paths_to_path_file() {
  local extraPathFilesDir="${DOTFILES_COPY}/path_files"
  local os="$(uname -s)"
  local extraPathsFile

  log_info "Looking for extra executable paths to add to \$PATH..."
  case "${os}" in
    "Darwin")
      log_info "Adding extra executable paths for macOS."
      extraPathsFile="${extraPathFilesDir}/mac_path"
      ;;
  esac

  if [ -f "${extraPathsFile}" ]; then
    cat "${extraPathsFile}" >>"${PATH_FILE}"
    rmduplines "${PATH_FILE}"
  fi
}

# Make sure ~/.path is up-to-date with default paths from the dotfiles
# repository.
pathsync() {
  cat "${DOTFILES_COPY}/.path" >>"${PATH_FILE}"
  add_extra_paths_to_path_file
  rmduplines "${PATH_FILE}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
