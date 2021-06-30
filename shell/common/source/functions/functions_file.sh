#!/bin/sh

### Create ###

mksource() {
  local sourceDir="$DOTFILES_HOME/shell/common/source"
  local sourceFileName="$*"
  local targetFile
  local response

  while [ -z "${sourceFileName}" ]; do
    echoe "Please enter name of new source file:"
    read -er sourceFileName
  done

  if ! echo "${sourceFileName}" | grep -E -q "(aliases)|(functions).*"; then
    err "Source file name must begin with \"aliases\" or \"functions\""
    return 2
  fi

  # Remove file extension from file name
  sourceFileName="${sourceFileName/.@(bash|zsh|sh)/}"

  if echo "${sourceFileName}" | grep -q "^aliases"; then
    targetFile="${sourceDir}/aliases/${sourceFileName}.sh"
  elif echo "${sourceFileName}" | grep -q "^functions"; then
    targetFile="${sourceDir}/functions/${sourceFileName}.sh"
  fi

  # Exit if the file already exists.
  if [ -f "$targetFile" ]; then
    err "File $targetFile already exists."
    return 3
  fi

  touch "${targetFile}"
  echo -e "#!/bin/sh\n\n\n" >>"${targetFile}"

  if echo "${sourceFileName}" | grep -q "^aliases"; then
    echo "# vim:foldenable:foldmethod=marker:foldlevel=0" >>"${targetFile}"
  elif echo "${sourceFileName}" | grep -q "^functions"; then
    echo "# vim:foldenable:foldmethod=indent:foldnestmax=1" >>"${targetFile}"
  fi
}

mkinit() {
  local newInit="$1"
  local initTemplate="${DOTFILES_HOME}/copy/templates/mkinit.bash"
  local initFileName
  local initFilePath

  if [ -z "$newInit" ]; then
    err "Must provide name for new init file"
    return 1
  fi

  initFileName="${newInit}"
  if ! echo "${initFileName}" | grep -q "^init[_\-]"; then
    initFileName="init_${initFileName}"
  fi
  if ! echo "${initFileName}" | grep -E -q "\.(sh)|(bash)"; then
    initFileName="${initFileName}"
  fi
  initFilePath="${DOTFILES_HOME}/init/${initFileName}"

  cp "${initTemplate}" "${initFilePath}"
  chmod 755 "${initFilePath}"
}

### Remove ###

# Remove all broken symlinks in a directory.
rmbsyml() {
  local dir="${1}"

  if [ ! -d "${dir}" ]; then
    err "${dir} is not a directory."
    return 1
  fi

  find -L "${dir}" -maxdepth 1 -type l -delete
}

# Remove duplicate lines from a given file.
rmduplines() {
  local fileToStripDuplicateLinesFrom="${1}"
  local tempFile="$(mktemp -u ${fileToStripDuplicateLinesFrom}.XXXXXXXXXX)"

  if [ ! -f "${fileToStripDuplicateLinesFrom}" ]; then
    err "${fileToStripDuplicateLinesFrom} is not a file."
    return 1
  fi

  awk '!x[$0]++' "${fileToStripDuplicateLinesFrom}" >"${tempFile}"
  mv "${tempFile}" "${fileToStripDuplicateLinesFrom}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
