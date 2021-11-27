#!/bin/sh

# Locate a given font via an input regular expression
findfont() {
  local pattern="$1"

  eval "fc-list | grep -E -i '${pattern}' | less"
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
