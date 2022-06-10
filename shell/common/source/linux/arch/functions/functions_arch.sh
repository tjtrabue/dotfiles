#!/bin/sh

# Install all packages listed in the packages file for Arch Linux.
# This makes bootstrapping new installations much easier.
install_arch_packages() {
  local archPackagesFile="${DOTFILES_HOME}/init/package_files/arch_packages.txt"

  grep -E -v -e '^\s*#' -e '^$' "${archPackagesFile}" |
    sudo pacman -S --needed -
}

# Re-initialize the Arch Linux GPG trust keyring. This is sometimes necessary
# after a long period of inactivity on an Arch Linux installation, as keys can
# become stale or untrusted, which can lead to system upgrades not completing.
pacrekey() {
  local archGnupgDir="/etc/pacman.d/gnupg"

  if [ -d "${archGnupgDir}" ]; then
    log_info "Removing Arch Linux GPG directory: ${BLUE}${archGnupgDir}${NC}"
    sudo rm -rf "${archGnupgDir}"
  fi

  log_info "Re-initializing Arch Linux GPG keyring"
  sudo pacman -Sy archlinux-keyring
  sudo pacman-key --init
  sudo pacman-key --populate archlinux
  sudo pacman-key --refresh-keys
}

# Clean up Arch installation, including package caches and orphaned packages.
paclean() {
  if [ -x "$(command -v paccache)" ]; then
    # `paccache` comes from the "pacman-contrib" package.
    paccache -r
  fi
  sudo pacman -Sc --noconfirm
  pacman_remove_orphans
}

# Remove the pacman lock file. This deadlock situation occurs sometimes when
# pacman is interrupted during an operation, such as the computer crashing
# during an update.
pacman_unlock_db() {
  local pacmanDbLockFile="/var/lib/pacman/db.lck"

  if [ -f "${pacmanDbLockFile}" ]; then
    log_info "Removing lock from pacman database:" \
      "${BLUE}${pacmanDbLockFile}${NC}"
    sudo rm -f "${pacmanDbLockFile}"
  fi
}

# Orphan = unused transitive package
pacman_remove_orphans() {
  pacman -Qtdq | sudo pacman -Rns --noconfirm -
}

# Set the Arch repository mirror list to only US sites
set_mirrorlist() {
  local mirrorlist="/etc/pacman.d/mirrorlist"
  local tempfile="$HOME/mirrorlist.tmp"

  if ! grep -q "^## United States" <"$mirrorlist"; then
    err "No United States mirrors found in mirrorlist"
    return 1
  fi

  grep -A 1 "## United States" <"$mirrorlist" |
    sed '/^## United States/d;/--/d' >"$tempfile"

  sudo mv "$mirrorlist"{,.bak}
  sudo cp "$tempfile" "$mirrorlist"
  rm -f "$tempfile"
}

# Figure out the proper backlight utility to use
install_brightness_util() {
  if [ "$(command -v xbacklight)" == "" ]; then
    sudo pacman -S xorg-xbacklight
  fi

  if ! xbacklight -inc 0 >>/dev/null; then
    log_info "No output device found for xbacklight; installing acpilight"
    sudo pacman -S acpilight

    {
      echo 'ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chgrp video
      /sys/class/backlight/%k/brightness"'
      echo 'ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chmod g+w
      /sys/class/backlight/%k/brightness"'
    } | sudo tee /etc/udev/rules.d/backlight.rules.tmp 1>/dev/null
  fi
}

editbootcfg() {
  sudoedit /etc/default/grub
}

upbootcfg() {
  sudo grub-mkconfig -o "$BOOTLOADER_CFG"
}

# Restart picom compositor
repicom() {
  pkill picom && sleep 1 && picom -b
}

# Update all Arch Linux packages, from both the standard and AUR repos.
uparch() {
  sudo pacman-key --refresh-keys
  aurhu
  sudo pacman -Syyu
}

# NOTE: You will need to reboot after installing optimus-manager
install_optimus_manager() {
  aurhinc "optimus-manager"
  # Copy the config file for editing later
  sudo cp "/usr/share/optimus-manager.conf" "/etc/optimus-manager/"
  # Enable the service
  sudo systemctl enable --now optimus-manager
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
