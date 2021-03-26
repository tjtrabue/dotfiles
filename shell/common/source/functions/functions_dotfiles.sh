#!/bin/sh

# Make sure all dotfile symlinks are present in $HOME directory
dotsync() {
  local dotfilesLink="${DOTFILES_LINK:-${DOTFILES_HOME}/link}"

  # Re-link all basic dotfiles
  find "${dotfilesLink}/home" -maxdepth 1 -mindepth 1 -type f \
    -exec ln -sf -t "${HOME}" '{}' \;

  # Finish by removing broken symlinks.
  rmbsyml "${HOME}"
}

# Create a new dotfile, link it to your home directory, and open it for editing
# in your configured editor.
# The input filename may have the '.' prefix or may omit the '.'. Any leading
# '.' characters will be stripped from the filename and replaced with a single
# '.' during file creation.
newdot() {
  # Pull in new file and strip leading dot, if present.
  local newFile="$(echo "${1}" | sed -r 's/^\.+//')"
  local newDotfileInRepo="${DOTFILES_HOME}/link/home/.${newFile}"
  local newDotfileInHome="${HOME}/.${newFile}"

  if [ -z "${newFile}" ]; then
    err "No filename given."
    return 1
  elif [ -f "${newDotfileInRepo}" ] || [ -h "${newDotfileInHome}" ]; then
    err "Dotfile .${newFile} already exists."
    return 2
  fi

  touch "${newDotfileInRepo}"
  ln -sf "${newDotfileInRepo}" "${newDotfileInHome}"

  edit "${newDotfileInHome}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
