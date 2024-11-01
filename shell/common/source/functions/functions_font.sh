#!/bin/sh

# Locate a given font via an input regular expression
findfont() {
  local pattern="$1"
  local searchCmd="grep -E -i"

  if [ -x "$(command -v rg)" ]; then
    searchCmd="rg -i"
  elif [ -x "$(command -v ag)" ]; then
    searchCmd="ag -i"
  fi

  searchCmd="${searchCmd} '${pattern}'"
  eval "fc-list | ${searchCmd} | sed 's/^[^:]*:\s*//' | less"
}

# Install or update the amazing Nerd Fonts repository.
sync_nerd_fonts() {
  local nerdFontsDestDir="${WS:-${HOME}/workspace}/nerd-fonts"

  __clone_fresh_nerd_fonts_repo

  if [ -d "${nerdFontsDestDir}" ]; then
    (
      cd "${nerdFontsDestDir}"
      ./install.sh
    )
  else
    err "Nerd fonts directory does not exist."
    return 1
  fi
}

# Install custom fonts from various locations.
install_custom_fonts() {
  local fontTempDir="/tmp/font"
  local userFontsDir="${HOME}/.local/share/fonts"
  local userFontsOtfDir="${userFontsDir}/otf"
  local userFontsTtfDir="${userFontsDir}/ttf"

  # Create user directories for OTF and TTF fonts
  mkdir -p "${userFontsOtfDir}" "${userFontsTtfDir}"

  # Install Neogrey font
  if [ ! -d "${userFontsOtfDir}/Neogrey" ]; then
    curl -sL --create-dirs \
      "https://dl.dafont.com/dl/?f=neogrey" -o "${fontTempDir}/neogrey/neogrey.zip"
    unzip "${fontTempDir}/neogrey/neogrey.zip"
    mkdir -p "${userFontsOtfDir}/Neogrey"
    cp -v -t "${userFontsOtfDir}/Neogrey" "${fontTempDir}/neogrey/"*.otf
  else
    log_info "Neogrey font directory already found at:" \
      "${BLUE}${userFontsOtfDir}/Neogrey${NC}"
  fi

  # Install Arkibal Serif font
  if [ ! -d "${userFontsOtfDir}/ArkibalSerif" ]; then
    curl -sL --create-dirs \
      "https://allbestfonts.com/wp-content/uploads/2017/12/elements-arkibal-serif-58UCG6-2017-03-03.zip" \
      -o "${fontTempDir}/arkibal_serif/arkibal_serif.zip"
    unzip "${fontTempDir}/arkibal_serif/arkibal_serif.zip"
    mkdir -p "${userFontsOtfDir}/ArkibalSerif" "${userFontsTtfDir}/ArkibalSerif"
    cp -v -t "${userFontsOtfDir}/ArkibalSerif" "${fontTempDir}/arkibal_serif/"*.otf
    cp -v -t "${userFontsTtfDir}/ArkibalSerif" "${fontTempDir}/arkibal_serif/"*.ttf
  else
    log_info "Arkibal Serif font directory already found at:" \
      "${BLUE}${userFontsOtfDir}/ArkibalSerif${NC}"
  fi

  # Install Quivira font
  if [ ! -d "${userFontsOtfDir}/Quivira" ]; then
    mkdir -p "${userFontsOtfDir}/Quivira"
    wget -qP "${userFontsOtfDir}/Quivira" "http://quivira-font.com/files/Quivira.otf"
  else
    log_info "Quivira font directory already found at:" \
      "${BLUE}${userFontsOtfDir}/Quivira${NC}"
  fi

  # Install Symbola font
  if [ ! -d "${userFontsTtfDir}/Symbola" ]; then
    mkdir -p "${userFontsTtfDir}/Symbola" "${fontTempDir}/symbola"
    wget -qP "${fontTempDir}" \
      "https://fontlibrary.org/assets/downloads/symbola/cf81aeb303c13ce765877d31571dc5c7/symbola.zip"
    unzip "${fontTempDir}/symbola/symbola.zip"
    cp -v -t "${userFontsTtfDir}/Symbola" "${fontTempDir}/symbola/"*.ttf
  else
    log_info "Symbola font directory already found at:" \
      "${BLUE}${userFontsTtfDir}/Symbola${NC}"
  fi

  # Update font cache
  fc-cache -rfv

  # Cleanup
  rm -rf "${fontTempDir}"
}

__clone_fresh_nerd_fonts_repo() {
  local nerdFontsGitUrl="https://github.com/ryanoasis/nerd-fonts.git"
  local nerdFontsDestDir="${WS:-${HOME}/workspace}/nerd-fonts"

  if [ -d "${nerdFontsDestDir}" ]; then
    log_info "Removing old Nerd Fonts repository"
    rm -rf "${nerdFontsDestDir}"
  fi

  log_info "Cloning Nerd Fonts repository to: ${BLUE}${nerdFontsDestDir}${NC}"
  git clone --depth 1 "${nerdFontsGitUrl}" "${nerdFontsDestDir}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
