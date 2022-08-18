#!/usr/bin/env bash

# WARNING: This script will destory data on the selected disk.
# This script can be run by executing the following:
#   curl -sL https://git.io/JLQlu | bash

set -uo pipefail
trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR

# Set up logging {{{
exec 1> >(tee "stdout.log")
exec 2> >(tee "stderr.log")
# }}}

# Variable declarations {{{
# URLs {{{
declare DOTFILES_REPO_URL="https://github.com/tjtrabue/dotfiles.git"
declare DOTFILES_RAW_URL="https://raw.github.com/tjtrabue/dotfiles"
declare DOTFILES_GIT_URL="git@github.com:tjtrabue/dotfiles.git"
declare PACKAGE_URL="${DOTFILES_RAW_URL}/develop/init/package_files/arch_packages.txt"
# }}}

# Input variables: {{{
declare IS_LAPTOP=1
declare USER_HOSTNAME=""
declare USERNAME=""
declare PASSWORD=-1
declare PASSWORD2=-2
declare -a DEVICE_LIST=($(lsblk -dplnx size -o name,size | grep -Ev "boot|rpmb|loop" | tac))
declare DEVICE=""
# }}}

# Filesystem variables: {{{
declare BOOT_SIZE=0
declare BOOT_END=0
declare SWAP_SIZE=0
declare SWAP_END=0
declare ROOT_SIZE=0
declare ROOT_END=0
declare PART_BOOT=""
declare PART_SWAP=""
declare PART_ROOT=""
declare PART_HOME=""
# }}}

# Mount point variables: {{{
declare MOUNT_ROOT="/mnt"
declare MOUNT_BOOT="${MOUNT_ROOT}/efi"
declare MOUNT_HOME="${MOUNT_ROOT}/home"
# }}}

# Directory variables {{{
declare USER_HOME=""
declare WORKSPACE=""
declare DOTFILES_HOME=""
declare DOTFILES_INIT=""
declare EFI_DIR="/sys/firmware/efi/efivars"
# }}}

# File variables {{{
declare XINIT_FILE=""
# }}}

# UEFI/BIOS {{{
declare UEFI_ENABLED=""
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
# Install packages necessary for this script to work.
install_necessary_packages() {
  pacman -Sy --noconfirm dialog
}

setup() {
  install_necessary_packages
}

cleanup() {
  info_log "Cleaning up"
  umount -R "$MOUNT_ROOT"
}
trap cleanup EXIT
# }}}

# Input functions {{{
get_computer_type() {
  dialog --stdout --yesno "Is this computer a laptop?" 0 0
  IS_LAPTOP=$?
  clear
}

get_hostname() {
  local response=1
  while [ $response -ne 0 ]; do
    USER_HOSTNAME="$(dialog --stdout --inputbox "Enter hostname" 0 0)"
    dialog --stdout --yesno "You entered ${USER_HOSTNAME}. Is that correct?" 0 0
    response=$?
  done
  clear
}

get_username() {
  local response=1
  while [ $response -ne 0 ]; do
    USERNAME="$(dialog --stdout --inputbox "Enter admin username" 0 0)"
    dialog --stdout --yesno "You entered ${USERNAME}. Is that correct?" 0 0
    response=$?
  done
  clear
}

get_password() {
  local match=true
  while [ "$PASSWORD" != "$PASSWORD2" ]; do
    if ! $match; then
      PASSWORD="$(dialog --stdout --passwordbox "Passwords do not match! Enter admin password" 0 0)"
    else
      PASSWORD="$(dialog --stdout --passwordbox "Enter admin password" 0 0)"
    fi

    if [ -z "$PASSWORD" ]; then
      clear
      echo "ERROR: Password cannot be empty" 2>&1
      continue
    fi

    PASSWORD2="$(dialog --stdout --passwordbox "Re-enter admin password" 0 0)"
    [ "$PASSWORD" != "$PASSWORD2" ] && match=false
  done
  clear
}

get_device() {
  local response=1
  while [ $response -ne 0 ]; do
    DEVICE="$(dialog --stdout --menu "Select installation disk" 0 0 0 "${DEVICE_LIST[@]}")"
    dialog --stdout --yesno "You selected ${DEVICE}. Is that correct?" 0 0
    response=$?
  done
  clear
}
# }}}
# }}}

# Setup {{{
setup
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
USER_HOME="/home/${USERNAME}"
WORKSPACE="${USER_HOME}/workspace"
DOTFILES_HOME="${WORKSPACE}/dotfiles"
DOTFILES_INIT="${DOTFILES_HOME}/init"

# Files
XINIT_FILE="${USER_HOME}/.xinitrc"
# }}}

# Determine if motherboard uses UEFI or BIOS {{{
if [ -d "${EFI_DIR}" ]; then
  info_log "UEFI mode enabled! Running in UEFI mode"
  UEFI_ENABLED=true
else
  info_log "Running in BIOS mode"
  UEFI_ENABLED=false
fi
# }}}

# Set NTP to keep system clock in sync
timedatectl set-ntp true

# Setup the disk and partitions {{{
# Start by clearing all existing signatures and data on the device
wipefs -af "${DEVICE}"

# Boot size in MiB
BOOT_SIZE=260
BOOT_END=$((BOOT_SIZE + 1))
# Swap size in GiB
SWAP_SIZE=$(free --gibi | awk '/Mem:/ {print $2}')
SWAP_END=$((SWAP_SIZE))
# Root size given in GiB
ROOT_SIZE=120
ROOT_END=$((SWAP_END + ROOT_SIZE + 1))

# The label for the storage device. This depends on whether or not UEFI mode is
# enabled.
diskLabel="msdos"
bootPartType="primary"
bootPartFs="ext4"
if ${UEFI_ENABLED}; then
  diskLabel="gpt"
  bootPartType="ESP"
  bootPartFs="fat32"
fi

parted --script "${DEVICE}" -- \
  mklabel "${diskLabel}" \
  mkpart "${bootPartType}" "${bootPartFs}" 1MiB ${BOOT_END} \
  set 1 boot on \
  mkpart primary linux-swap ${BOOT_END}MiB ${SWAP_END}GiB \
  mkpart primary ext4 ${SWAP_END}GiB ${ROOT_END}GiB \
  mkpart primary ext4 ${ROOT_END}GiB 100%

PART_BOOT="$(find "$(dirname "${DEVICE}")" -type b -regex "${DEVICE}p?1")"
PART_SWAP="$(find "$(dirname "${DEVICE}")" -type b -regex "${DEVICE}p?2")"
PART_ROOT="$(find "$(dirname "${DEVICE}")" -type b -regex "${DEVICE}p?3")"
PART_HOME="$(find "$(dirname "${DEVICE}")" -type b -regex "${DEVICE}p?4")"

# Create file systems on each partition.
if $UEFI_ENABLED; then
  mkfs.vfat -F32 "${PART_BOOT}"
else
  mkfs.ext4 "${PART_BOOT}"
fi
mkswap "${PART_SWAP}"
mkfs.ext4 "${PART_ROOT}"
mkfs.ext4 "${PART_HOME}"
# }}}

# Mount partitions {{{
info_log "Mounting partitions"

# Swap on the designated partition
swapon "${PART_SWAP}"

# Mount root
mount "${PART_ROOT}" "${MOUNT_ROOT}"

# Mount boot
mkdir -p "${MOUNT_BOOT}"
mount "${PART_BOOT}" "${MOUNT_BOOT}"

# Mount home
mkdir -p "${MOUNT_HOME}"
mount "${PART_HOME}" "${MOUNT_HOME}"
# }}}

# Install base packages {{{
info_log "Installing base packages"
pacstrap "${MOUNT_ROOT}" base linux linux-firmware base-devel
# }}}

# Host/locale information {{{
info_log "Configuring host/locale information"

genfstab -U "${MOUNT_ROOT}" >>"${MOUNT_ROOT}/etc/fstab"
echo "${USER_HOSTNAME}" >"${MOUNT_ROOT}/etc/hostname"

arch-chroot "${MOUNT_ROOT}" ln -sf /usr/share/zoneinfo/US/Central /etc/localtime
arch-chroot "${MOUNT_ROOT}" hwclock --systohc
arch-chroot "${MOUNT_ROOT}" sed -i -E 's/#(en_US\.UTF-8\sUTF-8)/\1/' /etc/locale.gen
arch-chroot "${MOUNT_ROOT}" locale-gen

echo "LANG=en_US.UTF-8" >"${MOUNT_ROOT}/etc/locale.conf"
# }}}

# Add admin user and set password {{{
info_log "Configuring users and groups"
info_log "Adding user ${USERNAME}"
arch-chroot "${MOUNT_ROOT}" useradd -mU -G \
  wheel,uucp,video,audio,storage,games,input "${USERNAME}"

info_log "Changing pasword for ${USERNAME}"
arch-chroot "${MOUNT_ROOT}" chpasswd <<<"${USERNAME}:${PASSWORD}"
arch-chroot "${MOUNT_ROOT}" chpasswd <<<"root:${PASSWORD}"
# }}}

# Configure the sudoers file {{{
info_log "Configuring sudoers file"
arch-chroot "${MOUNT_ROOT}" sed -i \
  's/^#\s*%wheel\s*ALL=(ALL)\s*NOPASSWD:\s*ALL/%wheel ALL=(ALL) NOPASSWD: ALL/' \
  /etc/sudoers

# Make sure the user's sudo password propogates to all open terminals
arch-chroot "${MOUNT_ROOT}" echo "Defaults !tty_tickets" >>/etc/sudoers
# }}}

# Configure pacman.conf file {{{
# Turn on pacman coloring
arch-chroot "${MOUNT_ROOT}" sed -i -E 's/#(Color)/\1/' \
  /etc/pacman.conf

cat <<EOF >>"${MOUNT_ROOT}/etc/pacman.conf"

# Enable 32-bit applications on your x86_64 system:
[multilib]
Include = /etc/pacman.d/mirrorlist
EOF
# }}}

# Install additional packages {{{
info_log "Installing additional packages"
# Grab packages file from dotfiles GitHub repository
arch-chroot "${MOUNT_ROOT}" pacman -Sy --needed --noconfirm - \
  <<<"$(curl -sL "${PACKAGE_URL}" | grep -E -v -e '^\s*#' -e '^$')"
# }}}

# Sound configuration {{{
# Create ALSA global configuration file
cat <<EOF >"${MOUNT_ROOT}/etc/asound.conf"
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

defaults.pcm.!card "PCH";
defaults.ctl.!card "PCH";
EOF

# Unmute sounds channels
arch-chroot "$MOUNT_ROOT" amixer sset Master unmute
arch-chroot "$MOUNT_ROOT" amixer sset Speaker unmute
arch-chroot "$MOUNT_ROOT" amixer sset Headphone unmute

# Turn volume all the way up
arch-chroot "$MOUNT_ROOT" amixer sset Master 100%
# }}}

# WiFi {{{
# wpa_supplicant
info_log "Configuring wpa_supplicant"
cat <<EOF >"${MOUNT_ROOT}/etc/wpa_supplicant/wpa_supplicant.conf"
ctrl_interface=/run/wpa_supplicant
update_config=1
EOF

# Activate WiFi services
info_log "Enabling NetworkManager and wpa_supplicant"
arch-chroot "$MOUNT_ROOT" systemctl enable wpa_supplicant
arch-chroot "$MOUNT_ROOT" systemctl enable NetworkManager
# }}}

# Configure bootloader {{{
# Install GRUB
info_log "Installing GRUB"
if ${UEFI_ENABLED}; then
  arch-chroot "${MOUNT_ROOT}" grub-install \
    --target=x86_64-efi \
    --efi-directory="/$(basename "${MOUNT_BOOT}")" \
    --bootloader-id=GRUB
else
  arch-chroot "${MOUNT_ROOT}" grub-install --target=i386-pc "${DEVICE}"
fi

# Generate GRUB config file
info_log "Configuring GRUB"
arch-chroot "$MOUNT_ROOT" grub-mkconfig -o /boot/grub/grub.cfg
# }}}

# Enable other services {{{
# CUPS for printer integration
arch-chroot "${MOUNT_ROOT}" systemctl enable cups

# Enable Network Time Protocol daemon
arch-chroot "${MOUNT_ROOT}" systemctl enable ntpd
# }}}

# Enable magic SysRq shorcuts {{{
info_log "Enabling magic SysRq key for maintenance"
cat <<EOF >"${MOUNT_ROOT}/etc/sysctl.d/99-default.conf"
kernel.sysrq = 1
EOF
# }}}

# Create .xinitrc file {{{
info_log "Configuring xinit"
cat <<EOF >"${MOUNT_ROOT}${XINIT_FILE}"
exec awesome
EOF
arch-chroot "${MOUNT_ROOT}" chown "${USERNAME}:${USERNAME}" "${XINIT_FILE}"
arch-chroot "${MOUNT_ROOT}" chmod 644 "${XINIT_FILE}"
# }}}

# Clone dotfiles {{{
info_log "Cloning dotfiles"
arch-chroot "${MOUNT_ROOT}" mkdir -p "${WORKSPACE}"
arch-chroot "${MOUNT_ROOT}" git clone "${DOTFILES_REPO_URL}" "${DOTFILES_HOME}"
arch-chroot "${MOUNT_ROOT}" chown -R "${USERNAME}:${USERNAME}" "${WORKSPACE}"
arch-chroot "${MOUNT_ROOT}" chmod -R a+rX "${WORKSPACE}"

# Change the dotfiles URL to the SSH version so that we may push commits to the
# repository later on.
# NOTE: 'git -C /path/to/repo' is how you run a git command for a repo without
#       actually being in the repo.
arch-chroot "${MOUNT_ROOT}" runuser "${USERNAME}" -c \
  "git -C ${DOTFILES_HOME} remote set-url origin ${DOTFILES_GIT_URL}"
# }}}

# Run installers {{{
info_log "Running install scripts"
arch-chroot "${MOUNT_ROOT}" runuser "${USERNAME}" -c \
  "bash ${DOTFILES_HOME}/install.sh --force"
# }}}

# Run init scripts {{{
info_log "Running initialization scripts for important topics"
arch-chroot "${MOUNT_ROOT}" runuser -l "${USERNAME}" -c \
  "runinit arch lightdm emacs neovim nerd_fonts awesome shell zsh docker asdf"
# }}}

# Run language-specific package install scripts {{{
info_log "Running language-specific package installations"
arch-chroot "${MOUNT_ROOT}" runuser -l "${USERNAME}" -c \
  "install_python_packages
  install_node_packages
  install_ruby_packages
  install_lua_packages
  install_go_packages"
# }}}

# Change shell {{{
# Change user's shell to Zsh.
arch-chroot "${MOUNT_ROOT}" chsh -s "/bin/zsh" "${USERNAME}"
# }}}

# Laptop configuration (if computer is a laptop) {{{
if [ "$IS_LAPTOP" -eq 0 ]; then
  info_log "Performing laptop configuration"
  arch-chroot "${MOUNT_ROOT}" runuser -l "${USERNAME}" -c \
    "bash ${DOTFILES_INIT}/bootstrap/configure_arch_laptop.sh"
fi
# }}}

# Final message {{{
cat <<EOF

Done!
Please take some time to ensure that all desired services are enabled and all
configurations are correct. Don't forget to reboot the system after you're
satisfied with your new Arch Linux installation.

To test your installation as your newly configured user, run the following:
  mount ${PART_ROOT} ${MOUNT_ROOT}
  mount ${PART_HOME} ${MOUNT_HOME}
  arch-chroot ${MOUNT_ROOT}
  su --login - ${USERNAME}
EOF
# }}}

# vim:foldenable:foldmethod=marker:
