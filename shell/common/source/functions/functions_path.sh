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
  local evaluatedPathToAdd="$(eval echo "${pathToAdd}")"
  pathToAdd="$(shortpath "${pathToAdd}")"

  if [ ! -d "${evaluatedPathToAdd}" ]; then
    err "Path ${BLUE}${pathToAdd}${NC} is not a directory"
    return 1
  fi

  if ! __evaluate_paths | grep -Fxq "${evaluatedPathToAdd}"; then
    log_debug "Adding path ${CYAN}${pathToAdd}${NC} to file" \
      "${GREEN}${pathFile}${NC}"
    printf "\n%s\n" "${pathToAdd}" >>"${pathFile}"
    rmblanklines "${pathFile}"
  fi

  # Regenerate the static path file.
  export_path "${pathFile}"
  # Source the static path file into the shell.
  spath "${pathFile}"
}

# Replace a leading portion of a given file path with a variable name
# corresponding to the longest possible prefix. For instance, if ~/.dirs
# contains:
#   dot="/home/me/.dotfiles",
# then
#   shortpath "/home/me/.dotfiles/link/emacs" -> ${dot}/link/emacs
#
# Also, if ~/.vars contains:
#   export JENV_HOME="/home/me/.jenv",
# then
#   shortpath "/home/me/.jenv/bin" -> ${JENV_HOME}/bin
shortpath() {
  local inputPath="${1}"
  local dirAliasFile="${DIR_ALIAS_FILE:-${HOME}/.dirs}"
  local varFile="${VAR_FILE:-${HOME}/.vars}"
  local bestVar=""
  local bestToReplace=""
  local var
  local varName
  local evaluatedPath=""
  local shortPath="${inputPath}"

  if [ -z "${inputPath}" ]; then
    err "No file path provided"
    return 1
  fi

  while IFS="" read -r var || [ -n "${var}" ]; do
    # Turn "export varName=varValue" into "varName"
    varName="${var#export }"
    varName="${varName%=*}"

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
  done < <(
    grep '^\s*export' "${varFile}" | sed 's/^\s*//'
    grep -v -e '^$' -e '^\s*#' "${dirAliasFile}" | sed 's/^\s*//'
  )

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
#
# This function uses a hashing algorithm to determine whether or not the entries
# in the given path file have changed (added, removed, or modified), and if they
# have, this function will re-create the static path file to keep it up-to-date
# with the entries in the path file.
spath() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local staticPathFile="$(__get_static_path_file_for_path_file "${pathFile}")"
  local pathHashFile="${pathFile}_hash"
  local pathHash

  if [ -f "${pathHashFile}" ]; then
    pathHash="$(__spath_get_path_file_hash)"
  else
    __spath_write_path_file_hash
    export_path "${pathFile}"
  fi

  if [ -n "${pathHash}" ] &&
     [ "${pathHash}" != "$(__spath_generate_hash_for_path_file)" ]; then
    export_path "${pathFile}"
    __spath_write_path_file_hash
  fi

  # Make sure the static path file exists. If not, create it.
  if [ ! -f "${staticPathFile}" ]; then
    export_path "${pathFile}"
  fi

  log_debug "Sourcing static path file: ${MAGENTA}${staticPathFile}${NC}"
  . "${staticPathFile}"
}

# Retrieve the hash digest for the PATH file, if the hash exists. If not, return
# an error code.
__spath_get_path_file_hash() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local pathHashFile="${pathFile}_hash"

  if [ ! -f "${pathHashFile}" ]; then
    err "No PATH hash file found at: ${BLUE}${pathHashFile}${NC}"
    return 1
  fi

  cat "${pathHashFile}" | tr -d '\n'
}

# Write the hash digest for a given path file (or ~/.path by default) to a file.
__spath_write_path_file_hash() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local pathHashFile="${pathFile}_hash"
  local hash="$(__spath_generate_hash_for_path_file "${pathFile}")"

  log_info "Writing hash ${CYAN}${hash}${NC} to: ${BLUE}${pathHashFile}${NC}"
  printf '%s' "${hash}" >"${pathHashFile}"
}

# Generate a hash digest for for a given path file (or ~/.path by default).
__spath_generate_hash_for_path_file() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"

  md5 "${pathFile}" | awk '{print $4}'
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
  __evaluate_paths "${pathFile}" |
    sed "s|${HOME}|\${HOME}|" |
    tr '\n' ':' |
    sed 's/:$//'
}

# Source the LuaRocks module path.
src_luarocks_module_path() {
  if [ -x "$(command -v luarocks)" ]; then
    eval "$(luarocks path)"
  fi
}

# Write additional paths to the ~/.path file depending on the operating system.
add_extra_paths_to_path_file() {
  local extraPathFilesDir="${DOTFILES_COPY}/path_files"
  local os="$(uname -s)"
  local extraPathsFile

  log_info 'Looking for extra executable paths to add to $PATH...'
  case "${os}" in
  "Darwin")
    log_info "Adding extra executable paths for macOS."
    extraPathsFile="${extraPathFilesDir}/mac_path"
    ;;
  *)
    log_info "No extra executable paths found."
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
  local dotPath="${DOTFILES_COPY}/dotfiles_to_copy/.path"

  # Backup the current path file just in case.
  if [ -f "${pathFile}" ]; then
    log_debug "Backing up path file ${pathFile} to ${pathFile}.bak"
    cp -f "${pathFile}"{,.bak}
  fi

  # Write non-commented, non-empty lines to ~/.path
  grep -E -v -e '^\s*#' -e '^$' "${dotPath}" >>"${pathFile}"
  add_extra_paths_to_path_file
  rmduplines "${pathFile}"
}

# Write the dynamically generated $PATH variable to a static file for ease of
# sourcing within a shell session. Evaluating $PATH dynamically every time a
# shell starts is slow and unnecessary, since the path file in question rarely
# changes.
export_path() {
  local binPathFile=${PATH_FILE:-${HOME}/.path}
  local pathFile="${1:-${binPathFile}}"
  local pathVarName="${2:-PATH}"
  local staticPathFile="$(__get_static_path_file_for_path_file "${pathFile}")"
  local rcFiles=(
    "${LEAN_PROFILE:-${HOME}/.lean_profile}"
  )
  local pathValue
  local f

  pathValue="$(construct_path "${pathFile}")"

  log_debug "Generating static path file: ${MAGENTA}${staticPathFile}${NC}"
  command cat <<EOF >"${staticPathFile}"
#!/bin/sh

# This file was automatically generated from the entries in '${pathFile}'.
# Please do not edit this file directly as it will eventually be regenerated
# and you will likely lose your changes. Instead, edit the entries in:
#   ${pathFile}
# and then run:
#   export_path ${pathFile} ${pathVarName}
# in your shell to recreate this file.

${pathVarName}="${pathValue}"
export ${pathVarName}
EOF

  # Replace all 'export PATH' directives in shell relevant initialization files
  # with updated $PATH value.
  for f in "${rcFiles[@]}"; do
    if [ -f "${f}" ]; then
      log_debug "Exporting ${pathVarName} to shell init file: ${BLUE}${f}${NC}"
      sed -E -i --follow-symlinks \
        "s|^(\s*export ${pathVarName}=).*|\1\"${pathValue}\"|" "${f}"
    else
      warn "Shell file ${BLUE}${f}${NC} does not exist"
    fi
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
