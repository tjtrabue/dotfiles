#!/bin/sh

# X11 window system functions.

# Configure basic X11 settings for a user's session. This should not have to be
# invoked repeatedly, so do not call it from the functions common.sh
# These settings should be safe to set for any X11 session, but are most useful
# in minimal environments, such as BSPWM or i3.
src_x11_definitions() {
  set_xmodmap_bindings
  set_xresources
  xset_settings
}

# Certain X keybindings are set via an antiquated system called Xmodmap.
set_xmodmap_bindings() {
  local userXmodmapFile="${HOME}/.Xmodmap"
  xmodmap "${userXmodmapFile}"
}

set_xresources() {
  local userXresourcesFile="${HOME}/.Xresources"
  xrdb "${userXresourcesFile}"
}

xset_settings() {
  local keyPressDelay="190"
  local keyRepeats="55"

  xset r rate "${keyPressDelay}" "${keyRepeats}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
