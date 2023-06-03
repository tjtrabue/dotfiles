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
  curl -sL --create-dirs \
    "https://dl.dafont.com/dl/?f=neogrey" -o "${fontTempDir}/neogrey/neogrey.zip"
  unzip "${fontTempDir}/neogrey/neogrey.zip"
  mkdir -p "${userFontsOtfDir}/Neogrey"
  cp -v -t "${userFontsOtfDir}/Neogrey" "${fontTempDir}/neogrey/"*.otf

  # Install Arkibal Serif font
  curl -sL --create-dirs \
    "https://allbestfonts.com/wp-content/uploads/2017/12/elements-arkibal-serif-58UCG6-2017-03-03.zip" \
    -o "${fontTempDir}/arkibal_serif/arkibal_serif.zip"
  unzip "${fontTempDir}/arkibal_serif/arkibal_serif.zip"
  mkdir -p "${userFontsOtfDir}/ArkibalSerif" "${userFontsTtfDir}/ArkibalSerif"
  cp -v -t "${userFontsOtfDir}/ArkibalSerif" "${fontTempDir}/arkibal_serif/"*.otf
  cp -v -t "${userFontsTtfDir}/ArkibalSerif" "${fontTempDir}/arkibal_serif/"*.ttf

  # Update font cache
  fc-cache -fv

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
