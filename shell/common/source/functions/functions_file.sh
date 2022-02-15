#!/bin/sh

# Create a new sourceable file (aliases, functions, etc.) for a given shell.
# The name of the file must begin with one of:
#   aliases_
#   functions_
# Use option flags to specify the shell for which the file should be created:
#   -c -> Common
#   -b -> Bash
#   -z -> Zsh
mksource() {
  local sourceFileName
  local dotfilesShellDir="${DOTFILES_HOME}/shell"
  local sourceDir="${dotfilesShellDir}/common/source"
  local shellType="common"
  local fileExtension="sh"
  local shebang="#!/bin/sh"
  local targetFileParentDir
  local targetFile
  local response
  local OPTIND
  local o

  while getopts ":bcz" o; do
    case "${o}" in
    b)
      shellType="bash"
      fileExtension="bash"
      shebang="#!/usr/bin/env bash"
      ;;
    c)
      shellType="common"
      fileExtension="sh"
      shebang="#!/bin/sh"
      ;;
    z)
      shellType="zsh"
      fileExtension="zsh"
      shebang="#!/usr/bin/env zsh"
      ;;
    *)
      err "Unknown option: ${o}"
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  case "${shellType}" in
  "bash")
    sourceDir="${dotfilesShellDir}/bash/source"
    ;;
  "common")
    sourceDir="${dotfilesShellDir}/common/source"
    ;;
  "zsh")
    sourceDir="${dotfilesShellDir}/zsh/source"
    ;;
  *)
    err "Could not determine source dir for shell type: ${shellType}"
    return 2
    ;;
  esac

  sourceFileName="$*"

  while [ -z "${sourceFileName}" ]; do
    echoe "Please enter name for new sourceable shell file:"
    read -er sourceFileName
  done

  if ! echo "${sourceFileName}" | grep -E -q "(aliases)|(functions).*"; then
    err 'Source file name must begin with "aliases" or "functions"'
    return 2
  fi

  # Remove file extension from input file name
  sourceFileName="${sourceFileName/.@(bash|zsh|sh)/}"

  # Figure out where to put the new file
  if echo "${sourceFileName}" | grep -q "^aliases"; then
    targetFileParentDir="${sourceDir}/aliases"
  elif echo "${sourceFileName}" | grep -q "^functions"; then
    targetFileParentDir="${sourceDir}/functions"
  fi

  # Construct the final path for the new file
  targetFile="${targetFileParentDir}/${sourceFileName}.${fileExtension}"

  # Exit if the file already exists.
  if [ -f "${targetFile}" ]; then
    err "File ${BLUE}${targetFile}${NC} already exists."
    return 3
  fi

  # Ensure the parent directory is present, and bring the file into existence.
  mkdir -p "${targetFileParentDir}"
  touch "${targetFile}"

  # Add the shebang to the top of the file
  printf '%s\n\n\n' "${shebang}" >>"${targetFile}"

  # Add Vim modeline to the bottom of the file
  if echo "${sourceFileName}" | grep -q "^aliases"; then
    printf '%s' "# vim:foldenable:foldmethod=marker:foldlevel=0" >>"${targetFile}"
  elif echo "${sourceFileName}" | grep -q "^functions"; then
    printf '%s' "# vim:foldenable:foldmethod=indent:foldnestmax=1" >>"${targetFile}"
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

# Remove all broken symlinks in a directory.
rmbsyml() {
  local dir="${1}"

  if [ ! -d "${dir}" ]; then
    err "${dir} is not a directory."
    return 1
  fi

  find -L "${dir}" -maxdepth 1 -type l -delete
}

# Remove duplicate lines from a given file or stdin if no file provided.
rmduplines() {
  # Read from stdin if no file argument provided.
  local fileToStripDuplicateLinesFrom="${1:-/dev/stdin}"
  # Temporary file to use for storing stripped contents.
  local tempFile="$(mktemp -u /tmp/rmduplines-XXXXXXXXXX)"

  if [ -f "${fileToStripDuplicateLinesFrom}" ]; then
    # If we're dealing with a standard file, write the stripped contents to a
    # temp file and then replace the original file with the newly stripped file.
    awk '!x[$0]++' "${fileToStripDuplicateLinesFrom}" >"${tempFile}" &&
      mv "${tempFile}" "${fileToStripDuplicateLinesFrom}" &&
      rm -f "${tempFile}"
  else
    # If we're only dealing with contents from stdin, write stipped contents
    # to stdout.
    awk '!x[$0]++' "${fileToStripDuplicateLinesFrom}"
  fi
}

# Remove empty lines from a file or stdin if no filename given. Note that this
# function considers lines containing only whitespace to be empty, also.
rmblanklines() {
  # Read from stdin if no file argument provided.
  local fileDescriptor="${1:-/dev/stdin}"

  if [ -f "${fileDescriptor}" ]; then
    sed -i '/^[[:space:]]*$/d' "${fileDescriptor}"
  else
    sed '/^[[:space:]]*$/d' "${fileDescriptor}"
  fi
}

# Remove the last newline (if present) from a given file. If no filename is
# given, or if the filename is '-', read from stdin and write to stdout.
rmfinalnewline() {
  local filename="${1:-/dev/stdin}"

  if [ "${filename}" = "-" ]; then
    filename="/dev/stdin"
  fi

  if [ "${filename}" = "/dev/stdin" ]; then
    printf %s "$(cat "${filename}")"
  else
    printf %s "$(cat "${filename}")" >"${filename}.bak"
    mv "${filename}"{.bak,}
  fi
}

# Return 0 if file1's contents are entirely contained within file2.
# Otherwise, return an error code.
#   file1: contained file
#   file2: containing file
filecontentsinfile() {
  local file1="${1}"
  local file2="${2}"

  if [ ! -f "${file1}" ] || [ ! -f "${file2}" ]; then
    err "Must provide two file arguments to filecontentsinfile"
    return 1
  fi

  # If any output is returned, file1 is not entirely contained within file2.
  if [ -n "$(
    comm -13 \
      <(sort -u "${file2}") \
      <(sort -u "${file1}") \
      2>/dev/null
  )" ]; then
    return 2
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
