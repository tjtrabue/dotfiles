#!/usr/bin/env bash

# Laptop-related packages {{{
install_laptop_packages() {
  sudo pacman -Sy --noconfirm \
    acpid \
    tlp \
    ;
}
# }}}

# Touchpad {{{
set_touchpad_options() {
  local xorgConfDir="/etc/X11/xorg.conf.d"
  local touchpadConfFile="${xorgConfDir}/90-touchpad.conf"

  sudo mkdir -p "$xorgConfDir"
  sudo tee $touchpadConfFile <<EOF >/dev/null
Section "InputClass"
  Identifier "touchpad"
  MatchIsTouchpad "on"
  Driver "libinput"
  Option "Tapping" "on"
  Option "TappingButtonMap" "lrm"
  Option "NaturalScrolling" "on"
  Option "ScrollMethod" "twofinger"
  Option "ClickMethod" "clickfinger"
EndSection

EOF
}
# }}}

# Service initiailization {{{
enable_services() {
  sudo systemctl enable --now acpid
  sudo systemctl enable --now tlp
}
# }}}

main() {
  install_laptop_packages
  set_touchpad_options
  enable_services
}

main
# vim:foldenable:foldmethod=marker:
