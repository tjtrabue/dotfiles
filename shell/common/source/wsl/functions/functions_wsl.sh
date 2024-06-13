#!/bin/sh

# Mount a Windows drive, such as "C:" (main disk) or "G:" (Google Drive) to
# a WSL2 filesystem.
mount_windows_drive() {
  local driveLetter="${1}"
  local winDriveSpecifier

  if [ -z "${driveLetter}" ]; then
    err "Must provide drive letter (i.e., 'c')"
    return 1
  elif ! echo "${driveLetter}" | grep -q '[a-z]'; then
    err "Argument must be a lowercase letter."
    return 1
  fi

  winDriveSpecifier="$(printf '%s' "${driveLetter}" | 
    tr '[:lower:]' '[:upper:]'):"

  sudo mount -t drvfs "${winDriveSpecifier}" "/mnt/${driveLetter}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1