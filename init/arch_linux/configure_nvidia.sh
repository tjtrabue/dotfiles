#!/usr/bin/env bash

# This script install packages and adds configuration necessary to use Arch
# Linux with the proprietary NVIDIA driver, which is a good deal more performant
# than the open-source nouveau driver that ships with Arch.

# Variables {{{
declare DOTFILES_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
declare THIS_EXEC="$(basename "${BASH_SOURCE[0]}")"

declare IMPORT="${DOTFILES_REPO}/bash/source"
# }}}

# Imports {{{
source "${IMPORT}/functions_log.bash"
source "${IMPORT}/linux/arch/functions_arch.bash"
# }}}

add_mkinitcpio_hook_for_nvidia() {
  local pacmanHooksDir="/etc/pacman.d/hooks"
  local nvidiaPacmanHook="${pacmanHooksDir}/nvidia.hook"

  log_info "Adding mkinitcpio hook for NVIDIA"
  sudo mkdir -p "${pacmanHooksDir}"

  sudo tee "${nvidiaPacmanHook}" <<EOF &>/dev/null
# This hook avoids the possibility of forgetting to update initramfs when
# the nvidia package updates

[Trigger]
Operation=Install
Operation=Upgrade
Operation=Remove
Type=Package
Target=nvidia
Target=linux
# Change the linux part above and in the Exec line if a different kernel is used

[Action]
Description=Update Nvidia module in initcpio
Depends=mkinitcpio
When=PostTransaction
NeedsTargets
Exec=/bin/sh -c 'while read -r trg; do case $trg in linux) exit 0; esac; done; /usr/bin/mkinitcpio -P'
EOF
}

make_nvidia_gpu_primary_rendering_device() {
  local nvidiaDrmXorgFile="/etc/X11/xorg.conf.d/10-nvidia-drm-outputclass.conf"
  sudo tee "${nvidiaDrmXorgFile}" <<EOF &>/dev/null
# Setup NVIDIA driver as the primary rendering provider for the Optimus device.
Section "OutputClass"
    Identifier "intel"
    MatchDriver "i915"
    Driver "modesetting"
EndSection

Section "OutputClass"
    Identifier "nvidia"
    MatchDriver "nvidia-drm"
    Driver "nvidia"
    Option "AllowEmptyInitialConfiguration"
    Option "PrimaryGPU" "yes"
    ModulePath "/usr/lib/nvidia/xorg"
    ModulePath "/usr/lib/xorg/modules"
EndSection
EOF
}

configure_optimus_manager() {
  local baseOptimusConfigFile="/usr/share/optimus-manager.conf"
  local mainOptimusConfigFile="/etc/optimus-manager/optimus-manager.conf"

  log_info "Configuring optimus-manager"

  # Copy the optimus-manager config file to the proper place and edit it.
  sudo cp -f "${baseOptimusConfigFile}" "${mainOptimusConfigFile}"
  sudo sed -i -r 's/^(startup_mode)=.*$/\1=auto/' "${mainOptimusConfigFile}"

  # Start up the optimus-manager systemd unit
  sudo systemctl enable --now optimus-manager
}

install_optimus_manager() {
  log_info "Installing optimus-manager"
  local optimusManagerPackage="optimus-manager"

  if [ -x "$(command -v "${AUR_HELPER}")" ]; then
    aurhinc "${optimusManagerPackage}"
  else
    warn "AUR helper not installed. Falling back on 'aur' function"
    aur install "${optimusManagerPackage}"
  fi
  configure_optimus_manager
}

install_arch_nvidia_packages() {
  log_info "Installing NVIDIA packages"
  pacman -S --noconfirm "nvidia" \
    "nvidia-prime" \
    "nvidia-settings" \
    "nvidia-utils" \
    "opencl-nvidia" \
    "xorg-server-devel"
}

install_and_configure_packages() {
  install_arch_nvidia_packages
  # install_optimus_manager
}

main() {
  print_header "Configuring Arch Linux for Nvidia"
  no_log_to_file
  install_and_configure_packages
  add_mkinitcpio_hook_for_nvidia
  make_nvidia_gpu_primary_rendering_device
}

main "$@"

# vim:foldenable:foldmethod=marker:foldlevel=0
