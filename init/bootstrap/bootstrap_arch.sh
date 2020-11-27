#!/usr/bin/env bash

# WARNING: This script will destory data on the selected disk.
# This script can be run by executing the following:
#   curl -sL https://git.io/fjHK3 | bash
set -uo pipefail
trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

# Set up logging {{{
exec 1> >(tee "stdout.log")
exec 2> >(tee "stderr.log")
# }}}

# Variable declarations {{{
# URLs {{{
declare dotfilesRepoUrl="https://github.com/tjtrabue/dotfiles.git"
declare dotfilesRawUrl="https://raw.github.com/tjtrabue/dotfiles"
declare dotfilesGitUrl="git@github.com:tjtrabue/dotfiles.git"
declare packageUrl="${dotfilesRawUrl}/develop/init/package_files/arch_packages.txt"
# }}}

# Input variables: {{{
declare isLaptop=1
declare hostname=""
declare user=""
declare password=-1
declare password2=-2
declare -a devicelist=($(lsblk -dplnx size -o name,size | grep -Ev "boot|rpmb|loop" | tac))
declare device=""
# }}}

# Filesystem variables: {{{
declare bootSize=0
declare bootEnd=0
declare swapSize=0
declare swapEnd=0
declare rootSize=0
declare rootEnd=0
declare partBoot=""
declare partSwap=""
declare partRoot=""
declare partHome=""
# }}}

# Mount point variables: {{{
declare mountRoot="/mnt"
declare mountBoot="${mountRoot}/efi"
declare mountHome="${mountRoot}/home"
# }}}

# Directory variables {{{
declare userHome=""
declare workspace=""
declare dotfilesHome=""
declare efiDir="/sys/firmware/efi/efivars"
# }}}

# File variables {{{
declare xinitFile=""
# }}}

# UEFI/BIOS {{{
declare uefiEnabled=""
# }}}
# }}}

# Helper functions {{{
# Logging functions {{{
log() {
  local prefix="$1"
  local msg="$2"
  local scriptName="$(basename "$0")"

  if [ -z "$prefix" ]; then
    printf "%s\n" "ERROR: No prefix given to ${FUNCNAME[0]}"
    return 1
  elif [ -z "$msg" ]; then
    printf "%s\n" "ERROR: No message given to ${FUNCNAME[0]}"
    return 2
  fi

  printf "[%s|%s] %s\n" "$prefix" "$scriptName" "$msg"
}

info_log() {
  log "INFO" "$*"
}

warn_log() {
  log "WARNING" "$*"
}

err_log() {
  log "ERROR" "$*"
}
# }}}

# Setup/cleanup {{{
cleanup() {
  info_log "Cleaning up"
  umount -R "$mountRoot"
}
trap cleanup EXIT
# }}}

# Input functions {{{
get_computer_type() {
  dialog --stdout --yesno "Is this computer a laptop?" 0 0
  isLaptop=$?
  clear
}

get_hostname() {
  local response=1
  while [ $response -ne 0 ]; do
    hostname="$(dialog --stdout --inputbox "Enter hostname" 0 0)"
    dialog --stdout --yesno "You entered ${hostname}. Is that correct?" 0 0
    response=$?
  done
  clear
}

get_username() {
  local response=1
  while [ $response -ne 0 ]; do
    user="$(dialog --stdout --inputbox "Enter admin username" 0 0)"
    dialog --stdout --yesno "You entered ${user}. Is that correct?" 0 0
    response=$?
  done
  clear
}

get_password() {
  local match=true
  while [ "$password" != "$password2" ]; do
    if ! $match; then
      password="$(dialog --stdout --passwordbox "Passwords do not match! Enter admin password" 0 0)"
    else
      password="$(dialog --stdout --passwordbox "Enter admin password" 0 0)"
    fi

    if [ -z "$password" ]; then
      clear
      echo "ERROR: Password cannot be empty" 2>&1
      continue
    fi

    password2="$(dialog --stdout --passwordbox "Re-enter admin password" 0 0)"
    [ "$password" != "$password2" ] && match=false
  done
  clear
}

get_device() {
  local response=1
  while [ $response -ne 0 ]; do
    device="$(dialog --stdout --menu "Select installation disk" 0 0 0 "${devicelist[@]}")"
    dialog --stdout --yesno "You selected ${device}. Is that correct?" 0 0
    response=$?
  done
  clear
}
# }}}
# }}}

# Get user input {{{
get_computer_type
get_hostname
get_username
get_password
get_device
# }}}

# Set directory and file variables {{{
# Directories
userHome="/home/${user}"
workspace="${userHome}/workspace"
dotfilesHome="${workspace}/dotfiles"

# Files
xinitFile="${userHome}/.xinitrc"
# }}}

# Determine if motherboard uses UEFI or BIOS {{{
if [ -d "$efiDir" ]; then
  info_log "UEFI mode enabled! Running in UEFI mode"
  uefiEnabled=true
else
  info_log "Running in BIOS mode"
  uefiEnabled=false
fi
# }}}

# Set NTP to keep system clock in sync
timedatectl set-ntp true

# Setup the disk and partitions {{{
# Start by clearing all existing signatures and data on the device
wipefs -af "${device}"

# Boot size in MiB
bootSize=260
bootEnd=$((bootSize + 1))
# Swap size in GiB
swapSize=$(free --gibi | awk '/Mem:/ {print $2}')
swapEnd=$((swapSize))
# Root size given in GiB
rootSize=80
rootEnd=$((swapEnd + rootSize + 1))

# The label for the storage device. This depends on whether or not UEFI mode is
# enabled.
diskLabel="msdos"
bootPartType="primary"
bootPartFs="ext4"
if $uefiEnabled; then
  diskLabel="gpt"
  bootPartType="ESP"
  bootPartFs="fat32"
fi

parted --script "${device}" -- \
  mklabel "$diskLabel" \
  mkpart "$bootPartType" "$bootPartFs" 1MiB ${bootEnd} \
  set 1 boot on \
  mkpart primary linux-swap ${bootEnd}MiB ${swapEnd}GiB \
  mkpart primary ext4 ${swapEnd}GiB ${rootEnd}GiB \
  mkpart primary ext4 ${rootEnd}GiB 100%

partBoot="$(find "$(dirname "$device")" -type b -regex "${device}p?1")"
partSwap="$(find "$(dirname "$device")" -type b -regex "${device}p?2")"
partRoot="$(find "$(dirname "$device")" -type b -regex "${device}p?3")"
partHome="$(find "$(dirname "$device")" -type b -regex "${device}p?4")"

# Create file systems on each partition.
if $uefiEnabled; then
  mkfs.vfat -F32 "${partBoot}"
else
  mkfs.ext4 "${partBoot}"
fi
mkswap "${partSwap}"
mkfs.ext4 "${partRoot}"
mkfs.ext4 "${partHome}"
# }}}

# Mount partitions {{{
info_log "Mounting partitions"

# Swap on the designated partition
swapon "${partSwap}"

# Mount root
mount "${partRoot}" "${mountRoot}"

# Mount boot
mkdir -p "${mountBoot}"
mount "${partBoot}" "${mountBoot}"

# Mount home
mkdir -p "${mountHome}"
mount "${partHome}" "${mountHome}"
# }}}

# Install base packages {{{
info_log "Installing base packages"
pacstrap "$mountRoot" base linux linux-firmware base-devel
# }}}

# Host/locale information {{{
info_log "Configuring host/locale information"

genfstab -U "$mountRoot" >>"$mountRoot"/etc/fstab
echo "${hostname}" >"$mountRoot"/etc/hostname

arch-chroot "$mountRoot" ln -sf /usr/share/zoneinfo/US/Eastern /etc/localtime
arch-chroot "$mountRoot" hwclock --systohc
arch-chroot "$mountRoot" sed -i -r 's/#(en_US\.UTF-8\sUTF-8)/\1/' /etc/locale.gen
arch-chroot "$mountRoot" locale-gen

echo "LANG=en_US.UTF-8" >"$mountRoot"/etc/locale.conf
# }}}

# Add admin user and set password {{{
info_log "Configuring users and groups"
info_log "Adding user $user"
arch-chroot "$mountRoot" useradd -mU -G wheel,uucp,video,audio,storage,games,input "$user"

info_log "Changing pasword for $user"
arch-chroot "$mountRoot" chpasswd <<<"${user}:${password}"
arch-chroot "$mountRoot" chpasswd <<<"root:${password}"
# }}}

# Configure the sudoers file {{{
info_log "Configuring sudoers file"
arch-chroot "$mountRoot" sed -i \
  's/^#\s*%wheel\s*ALL=(ALL)\s*NOPASSWD:\s*ALL/%wheel ALL=(ALL) NOPASSWD: ALL/' \
  /etc/sudoers

# Make sure the user's sudo password propogates to all open terminals
arch-chroot "$mountRoot" echo "Defaults !tty_tickets" >>/etc/sudoers
# }}}

# Install additional packages {{{
info_log "Installing additional packages"
# Grab packages file from github repo
arch-chroot "$mountRoot" pacman -S --needed --noconfirm - <<<"$(curl -sL "$packageUrl")"
# }}}

# Sound configuration {{{
# Create ALSA global configuration file
cat <<EOF >"${mountRoot}/etc/asound.conf"
# You may need to replace the card index value to a string value to improve
# consistency from one boot to another. The strings are the name of the card to
# use, like "PCH". You can list the names of your sound devices by issuing the
# command 'aplay -l'. I highly recommend you read the ALSA article on the Arch
# Wiki before changing this file because its syntax is quite tricky.
#
# NOTE: You must enclose the strings in double quotes.
#
# Example:
#   defaults.pcm.!card "PCH";
#   defaults.ctl.!card "PCH";

defaults.pcm.!card 0;
defaults.ctl.!card 0;
EOF

# Unmute sounds channels
arch-chroot "$mountRoot" amixer sset Master unmute
arch-chroot "$mountRoot" amixer sset Speaker unmute
arch-chroot "$mountRoot" amixer sset Headphone unmute

# Turn volume all the way up
arch-chroot "$mountRoot" amixer sset Master 100%
# }}}

# Configure network manager {{{
info_log "Configuring NetworkManager"
cat <<EOF >"${mountRoot}/etc/NetworkManager/conf.d/dhcp-client.conf"
[main]
dhcp=dhclient
EOF

arch-chroot "$mountRoot" systemctl enable NetworkManager
# }}}

# Configure bootloader {{{
# Install GRUB
info_log "Installing GRUB"
if $uefiEnabled; then
  arch-chroot "$mountRoot" grub-install \
    --target=x86_64-efi \
    --efi-directory="/$(basename "$mountBoot")" \
    --bootloader-id=GRUB
else
  arch-chroot "$mountRoot" grub-install --target=i386-pc "${device}"
fi

# Generate GRUB config file
info_log "Configuring GRUB"
arch-chroot "$mountRoot" grub-mkconfig -o /boot/grub/grub.cfg
# }}}

# Enable other services {{{
# CUPS for printer integration
arch-chroot "$mountRoot" systemctl enable org.cups.cupsd
# }}}

# Enable magic SysRq shorcuts {{{
info_log "Enabling magic SysRq key for maintenance"
cat <<EOF >"${mountRoot}/etc/sysctl.d/99-default.conf"
kernel.sysrq = 1
EOF
# }}}

# Create .xinitrc file {{{
info_log "Configuring xinit"
cat <<EOF >"${mountRoot}${xinitFile}"
exec i3
EOF
arch-chroot "$mountRoot" chown "${user}:${user}" "$xinitFile"
arch-chroot "$mountRoot" chmod 644 "$xinitFile"
# }}}

# Clone dotfiles {{{
info_log "Cloning dotfiles"
arch-chroot "$mountRoot" mkdir -p "$workspace"
arch-chroot "$mountRoot" git clone "$dotfilesRepoUrl" "$dotfilesHome"
arch-chroot "$mountRoot" chown -R "${user}:${user}" "$workspace"
arch-chroot "$mountRoot" chmod -R a+rX "$workspace"

# Change the dotfiles URL to the SSH version so that we may push commits to the
# repository later on.
# NOTE: 'git -C /path/to/repo' is how you run a git command for a repo without
#       actually being in the repo.
arch-chroot "$mountRoot" runuser "$user" -c \
  "git -C $dotfilesHome remote set-url origin $dotfilesGitUrl"
# }}}

# Run installers {{{
info_log "Running install scripts"
arch-chroot "$mountRoot" runuser "$user" -c \
  "bash ${dotfilesHome}/install.sh --force"
# }}}

# Run init scripts {{{
# Standard Arch configuration
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_arch"
# LightDM
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_lightdm"
# Emacs
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_emacs"
# Vim/Neovim
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_neovim"
# Nerd Fonts
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_nerd_fonts"
# Awesome WM
arch-chroot "$mountRoot" runuser -l "$user" -c \
  "bash ${dotfilesHome}/init/init_awesome"
# }}}

# Run language-specific package install scripts {{{
info_log "Running language-specific package installations"
arch-chroot "$mountRoot" runuser -l "$user" -c "install_python_packages"
arch-chroot "$mountRoot" runuser -l "$user" -c "install_npm_global_packages"
arch-chroot "$mountRoot" runuser -l "$user" -c "install_lua_packages"
arch-chroot "$mountRoot" runuser -l "$user" -c "install_go_packages"
# }}}

# Laptop configuration (if computer is a laptop) {{{
if [ "$isLaptop" -eq 0 ]; then
  info_log "Performing laptop configuration"
  arch-chroot "$mountRoot" runuser -l "$user" -c \
    "bash ${dotfilesHome}/init/bootstrap/configure_arch_laptop.sh"
fi
# }}}

# Final message {{{
cat <<EOF

Done!
Please take some time to ensure that all desired services are enabled and all
configurations are correct. Don't forget to reboot the system after you're
satisfied with your new Arch Linux installation.

To test your installation as your newly configured user, run the following:
  mount $partRoot $mountRoot
  mount $partHome $mountHome
  arch-chroot $mountRoot
  su --login - $user
EOF
# }}}

# vim:foldenable:foldmethod=marker:
