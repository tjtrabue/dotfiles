#!/bin/sh

# Clone and link terminal themes to appropriate directories.
init_terminal_themes() {
  log_info "Initializing terminal color schemes"

  __clone_iterm2_color_schemes
  __link_iterm2_color_schemes_to_other_terminal_theme_dirs
}

# Update the alacritty.yml file's colors based on a given theme name.
change_alacritty_theme() {
  local themeName="${1}"
  local alacrittyConfigFile="${USER_CONF:-${HOME}/.config}/alacritty/alacritty.yml"
  local alacrittyThemesDir="${WS}/iTerm2-Color-Schemes/alacritty"
  local newThemeFile="${alacrittyThemesDir}/${themeName}.yml"

  if [ -z "${themeName}" ]; then
    err "No theme name provided"
    return 1
  elif [ ! -d "${alacrittyThemesDir}" ]; then
    err "Could not find alacritty themes dir at:" \
      "${BLUE}${alacrittyThemesDir}${NC}"
    return 2
  elif [ ! -f "${newThemeFile}" ]; then
    err "Theme file ${BLUE}${newThemeFile}${NC} not found"
    return 3
  fi

  # Update the alacritty.yml file's "colors" attribute.
  yq eval-all -i 'select(fileIndex==0).colors = select(fileIndex==1) |
  select(fileIndex==0)' \
    "${alacrittyThemesDir}" \
    "${newThemeFile}"
}

__clone_iterm2_color_schemes() {
  local gitUrl="https://github.com/mbadolato/iTerm2-Color-Schemes.git"
  local destDir="${WS:-${HOME}/workspace}/$(basename "${gitUrl%.git}")"

  if [ -d "${destDir}" ]; then
    log_info "Updating iTerm2 Color Schemes repo"
    git -C "${destDir}" reset --hard
    git -C "${destDir}" clean -df
    git -C "${destDir}" pull
  else
    log_info "Cloning iTerm2 Color Schemes repo"
    git clone --depth 1 "${gitUrl}" "${destDir}"
  fi
}

# The iTerm2 Color Schemes repo contains ports for its themes to many other
# terminal emulators.
__link_iterm2_color_schemes_to_other_terminal_theme_dirs() {
  local iterm2ColorSchemesDir="${WS:-${HOME}/workspace}/iTerm2-Color-Schemes"

  log_info "Linking iTerm2 color schemes for other terminal emulators"
  __link_iterm2_kitty_themes "${iterm2ColorSchemesDir}"
}

# Link iTerm2 ported color schemes to Kitty's config directory.
__link_iterm2_kitty_themes() {
  local iterm2ColorSchemesDir="${1}"
  local kittyThemesDir="${HOME}/.config/kitty/kitty-themes"

  if __iterm2_color_schemes_dir_exists; then
    log_info "Linking iTerm2 color schemes for Kitty to:" \
      "${BLUE}${kittyThemesDir}${NC}"
    ln -sf "${iterm2ColorSchemesDir}/kitty" "${kittyThemesDir}"
  else
    err "Did not link Kitty themes"
    return 1
  fi
}

__iterm2_color_schemes_dir_exists() {
  local iterm2ColorSchemesDir="${WS:-${HOME}/workspace}/iTerm2-Color-Schemes"

  if [ ! -d "${iterm2ColorSchemesDir}" ]; then
    err "iTerm2-Color-Schemes repository not found at:" \
      "${BLUE}${iterm2ColorSchemesDir}${NC}"
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
