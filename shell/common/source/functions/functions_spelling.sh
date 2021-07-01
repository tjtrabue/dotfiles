#!/bin/sh

# Clones dictionaries used by spell-checking tools such as ispell or
# hunspell.
clone_ispell_dictionaries() {
  log_info "Cloning ispell dictionaries..."
  local repoUrl="https://github.com/LibreOffice/dictionaries.git"
  mkdir -p "${WS}"
  (
    cd "${WS}"
    git clone "${repoUrl}"
  )
}

# Installs ispell or hunspell dictionaries to standard locations, and clones the
# dictionaries if need be.
install_ispell_dictionaries() {
  log_info "Installing ispell dictionaries..."
  local dictionariesRepo="${WS}/dictionaries"
  local standardDictionariesDir="/usr/share/hunspell"

  if [ "$(getdistro)" = "Darwin" ]; then
    # Standard dictionaries direcotry is different for Mac computers.
    standardDictionariesDir="/Library/Spelling"
  fi

  if [ ! -d "${dictionariesRepo}" ]; then
    clone_ispell_dictionaries
  fi

  if [ ! -d "${standardDictionariesDir}" ]; then
    log_info "Creating dictionaries directory: ${standardDictionariesDir}"
    sudo mkdir -p "${standardDictionariesDir}"
  fi

  log_info "Copying dictionary files..."
  sudo cp "${dictionariesRepo}/en/en_US"* "${standardDictionariesDir}"/
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
