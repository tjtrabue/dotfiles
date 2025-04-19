#!/bin/sh

# Alias a given cursor theme to ~/.local/share/icons/default
set_cursor_theme() {
  local themeName="${1}"
  local systemCursorThemesRoot="/usr/share/icons"
  local systemThemeDir="${systemCursorThemesRoot}/${themeName}"
  local userDefaultThemeDir="${HOME}/.local/share/icons/default"

  if [ -z "${themeName}" ]; then
    err "No cursor theme name given"
    return 1
  elif [ ! -d "${systemThemeDir}" ]; then
    err "Cursor theme dir ${BLUE}${systemThemeDir}${NC} does not exist"
    return 1
  fi

  mkdir -p "$(dirname "${userDefaultThemeDir}")"

  if [ -h "${userDefaultThemeDir}" ]; then
    log_info "Unlinking old user theme"
    rm -f "${userDefaultThemeDir}"
  fi

  log_info "Linking cursor theme dir ${BLUE}${systemThemeDir}${NC} to" \
    "${BLUE}${userDefaultThemeDir}${NC}"
  ln -sf "${systemThemeDir}" "${userDefaultThemeDir}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1