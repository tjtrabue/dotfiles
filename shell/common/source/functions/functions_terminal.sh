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
  local newThemeFile

  if [ ! -x "$(command -v yq)" ]; then
    err "yq command line tool not found on PATH"
    return 1
  fi

  if [ ! -d "${alacrittyThemesDir}" ]; then
    warn "alacritty themes directory not found at at:" \
      "${BLUE}${alacrittyThemesDir}${NC}"
    init_terminal_themes
  fi

  # Allow the user to select a theme with a fuzzy finder program if they did not
  # supply a theme name on the command line.
  if [ -z "${themeName}" ]; then
    themeName="$(__fuzzy_select_theme_from_dir "${alacrittyThemesDir}")"
  fi

  # If themeName is still empty after prompting the user, assume the user wants
  # to exit the program.
  if [ -z "${themeName}" ]; then
    return 0
  fi

  # Put together the full path to the theme file
  newThemeFile="${alacrittyThemesDir}/${themeName}.yml"

  if [ ! -f "${newThemeFile}" ]; then
    err "Theme file ${BLUE}${newThemeFile}${NC} not found"
    return 3
  fi

  log_info "Updating Alacritty theme to: ${CYAN}${themeName}${NC}"

  # Update the alacritty.yml file's "colors" attribute.
  yq eval-all -i 'select(fileIndex==0).colors = select(fileIndex==1) |
  select(fileIndex==0)' \
    "${alacrittyConfigFile}" \
    "${newThemeFile}"
}

# Change the content of ~/.config/kitty/theme.conf to that of the given theme
# name.
change_kitty_theme() {
  local themeName="${1}"
  local kittyThemesDir="${WS}/iTerm2-Color-Schemes/kitty"
  local kittyThemeFile="${USER_CONF:-${HOME}/.config}/kitty/theme.conf"
  local newThemeFile

  if [ ! -d "${kittyThemesDir}" ]; then
    warn "Kitty themes directory not found at: ${BLUE}${kittyThemesDir}${NC}"
    init_terminal_themes
  fi

  if [ -z "${themeName}" ]; then
    themeName="$(__fuzzy_select_theme_from_dir "${kittyThemesDir}")"
  fi

  # If themeName is still empty after prompting the user, assume the user wants
  # to exit the program.
  if [ -z "${themeName}" ]; then
    return 0
  fi

  newThemeFile="${kittyThemesDir}/${themeName}.conf"

  if [ ! -f "${newThemeFile}" ]; then
    err "Theme file ${BLUE}${newThemeFile}${NC} not found"
    return 2
  fi

  # Copy the selected theme file to ~/.config/kitty/theme.conf
  cp -f "${newThemeFile}" "${kittyThemeFile}"
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

__fuzzy_select_theme_from_dir() {
  local themeDir="${1}"
  local fuzzyFindCmd
  local fileSearchCmd
  local finalSelectCmd

  if [ ! -d "${themeDir}" ]; then
    err "No theme directory found at: ${BLUE}${themeDir}${NC}"
    return 1
  fi

  if [ -x "$(command -v fzf)" ]; then
    fuzzyFindCmd="fzf"
  else
    fuzzyFindCmd="fzy"
  fi

  if [ -x "$(command -v fd)" ]; then
    fileSearchCmd="fd -t f --exec echo '{/.}' \; '.' ${themeDir}"
  else
    fileSearchCmd="find ${themeDir} -type f -exec basename '{}' \; | sed 's/\..*\$//'"
  fi

  finalSelectCmd="${fileSearchCmd} | sort | ${fuzzyFindCmd}"
  eval "${finalSelectCmd}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
