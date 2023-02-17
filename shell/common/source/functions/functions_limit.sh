#!/bin/sh

# It's often quite necessary to increase Linux's limit for open files per shell
# session, as the default is usually too small for modern applications. This
# low threshold can lead to problems where a user launches a large GUI
# application from the command line, only for the app to crash and the terminal
# to print a message stating something along the lines of "too many open file".
#
# You may need to reboot your computer after making these changes for them to
# take effect.
set_max_no_open_files() {
  local maxNoOpenFiles="${1:-51200}"
  local sysctlConfFile="/etc/sysctl.conf"

  log_info "Setting the max number of open files to:" \
    "${GREEN}${maxNoOpenFiles}${NC}"

  sudo sysctl -w fs.file-max="${maxNoOpenFiles}"

  # Replace fs.file-max rules if they exist; otherwise, create them.
  if grep -q '^\s*fs.file-max.*' "${sysctlConfFile}"; then
    sudo sed -i "s/fs.file-max.*/fs.file-max=${maxNoOpenFiles}/" \
      "${sysctlConfFile}"
  else
    printf '%s=%s' "fs.file-max" "${maxNoOpenFiles}" |
      sudo tee -a "${sysctlConfFile}" >>/dev/null 2>&1
  fi

  sudo sysctl -p
}

# vim:foldenable:foldmethod=indent:foldnestmax=1