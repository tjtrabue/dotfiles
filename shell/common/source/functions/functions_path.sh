#!/bin/sh

# Adds a given path (or the current working directory if no path is provided)
# to a given path file (or ~/.path if no path file is provided). After adding
# the new path to the path file, this function creates a static path file based
# on the entries in the given path file containing the export statement for use
# in the shell.
atp() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathToAdd="${1:-$(pwd)}"
  local pathFile="${2:-${binPathFile}}"
  pathToAdd="$(shortpath "${pathToAdd}")"

  if ! __evaluate_paths | grep -Fxq "$(eval echo "${pathToAdd}")"; then
    log_debug "Adding path ${CYAN}${pathToAdd}${NC} to file" \
      "${GREEN}${pathFile}${NC}"
    printf "%s\n" "${pathToAdd}" >>"${pathFile}"
  fi

  # Regenerate the static path file.
  export_path "${pathFile}"
  # Source the static path file into the shell.
  spath "${pathFile}"
}

# Replace a leading portion of a given file path with the diralias corresponding
# to the longest possible prefix. For instance, if ~/.dirs contains:
#
#   dot="/home/me/.dotfiles"
#
# then
#
#   shortpath "/home/me/.dotfiles/link/emacs" -> ${dot}/link/emacs
shortpath() {
  local inputPath="$1"
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local bestVar=""
  local bestToReplace=""
  local var
  local varName
  local varValue
  local evaluatedPath=""
  local shortPath="${inputPath}"

  if [ -z "${inputPath}" ]; then
    err "No file path provided."
    return 1
  fi

  while IFS="" read -r var || [ -n "${var}" ]; do
    if echo "${var}" | grep -q -e '^\s*#' -e '^$'; then
      # Ignore commented and empty lines
      continue
    fi

    varName=${var%=*}
    varValue=${var#*=}
    # Fully expand the variable's value to remove any environment variables from
    # the string.
    evaluatedPath="$(eval "echo \$${varName}" 2>/dev/null)"

    if [ -d "${evaluatedPath}" ]; then
      log_debug "\$${varName} is a directory variable"
      if echo "${inputPath}" | grep -q "^${evaluatedPath}" &&
      [ "${#evaluatedPath}" -ge "${#bestToReplace}" ]; then
        bestToReplace="${evaluatedPath}"
        bestVar="${varName}"
        log_debug "New best to replace: ${varName}=${bestToReplace}"
        shortPath="${inputPath/${bestToReplace}/\${${bestVar}\}}"
      fi
    fi
  done <"${dirAliasFile}"

  echo "${shortPath}"
}

# Read a path file's contents into a path variable, then export the path
# variable.
#
# @param [$1=~/.path] - The path file to read from
# @param [$2=PATH]    - The path variable to export
eval_path_var_from_file() {
  local pathFile="${1:-${PATH_FILE}}"
  local pathVar="${2:-PATH}"

  eval "${pathVar}=$(construct_path "${pathFile}")"
  export "${pathVar?}"
}

# Source a path variable from a static, automatically generated file into the
# current shell session. This is much faster than dynamically evaluating the
# path variable each time the variable's value is needed.
spath() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local staticPathFile="$(__get_static_path_file_for_path_file "${pathFile}")"

  # Make sure the static path file exists. If not, create it.
  if [ ! -f "${staticPathFile}" ]; then
    export_path "${pathFile}"
  fi

  log_debug "Sourcing static path file: ${MAGENTA}${staticPathFile}${NC}"
  . "${staticPathFile}"
}

# Echo a path variable (PATH by default) and its value to stdout.
epath() {
  local pathFile="${1:-${PATH_FILE}}"
  local pathVar="${2:-PATH}"
  printf "%s='%s'\n" "${pathVar}" "$(construct_path "${pathFile}")"
}

# Put together a path string from an input path file containing lines to join
# into a single path specifier.
construct_path() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"

  log_debug "Constructing path variable from file: ${GREEN}${pathFile}${NC}"
  __evaluate_paths "${pathFile}" | tr '\n' ':' | sed 's/:$//'
}

# Source the LuaRocks module path.
src_lua_path() {
  if [ -x "$(command -v luarocks)" ]; then
    eval "$(luarocks path)"
  fi
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
  local pathFile="${1:-${PATH_FILE}}"

  # Backup the current path file just in case.
  if [ -f "${pathFile}" ]; then
    log_info "Backing up path file: ${pathFile}"
    cp "${pathFile}"{,.bak}
  fi

  cat "${DOTFILES_COPY}/.path" >>"${pathFile}"
  add_extra_paths_to_path_file
  rmduplines "${pathFile}"
}

# Write the dynamically generated $PATH variable to a static file for ease of
# exporting within a shell session. Evaluating a dynamic path every time a shell
# starts is slow and unnecessary, since the path file in question rarely changes.
export_path() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local staticPathFile="$(__get_static_path_file_for_path_file "${pathFile}")"
  local rcFiles=(
    "${HOME}/.bash_profile"
    "${HOME}/.bashrc"
    "${HOME}/.zshenv"
    "${ZDOTDIR}/.zshrc"
  )
  local pathValue
  local f

  pathValue="$(construct_path "${pathFile}")"

  log_debug "Generating static path file: ${MAGENTA}${staticPathFile}${NC}"
  cat <<EOF >"${staticPathFile}"
#!/bin/sh

# This file was automatically generated from the entries in '${pathFile}'.
# Please do not edit this file directly as it will eventually be regenerated
# and you will likely lose your changes. Instead, edit the entries in:
#   '${pathFile}'
# and then run 'export_path' in your shell to recreate this file.

PATH="${pathValue}"
export PATH
EOF

  # Replace all 'export PATH' directives in shell relevant initialization files
  # with updated $PATH value.
  for f in "${rcFiles[@]}"; do
    log_debug "Exporting PATH to shell init file: ${BLUE}${f}${NC}"
    sed -E -i --follow-symlinks \
      "s|^(\s*export PATH=).*|\1\"${pathValue}\"|" "${f}"
  done
}

# Print paths in $PATH file with all environment variables/subshells evaluated
__evaluate_paths() {
  local pathFile="${1:-${PATH_FILE}}"
  local binPath
  local evaluatedPath

  # Make sure that paths are evaluated in reverse order from their listing in
  # the .path file since we want more recently added paths to take precedence
  # over older ones.
  # The grep command is to remove empty and commented lines.
  # The awk command at the end removes cuplicates from the listing.

  while IFS="" read -r binPath || [ -n "${binPath}" ]; do
    # Make sure only to output paths from the file that can be evaluated.
    evaluatedPath="$(eval "printf '%s\n' ${binPath}")"
    [ -n "${evaluatedPath}" ] && printf "%s\n" "${evaluatedPath}"
  done <"${pathFile}" | grep -v -E -e "^#.*" -e "^$" | tac | awk '!x[$0]++'
}

# Gets the static path file's path for a given standard path file.
__get_static_path_file_for_path_file() {
  local pathFile="${1:-${PATH_FILE}}"
  printf "%s" "${pathFile}_static"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
