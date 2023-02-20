#!/bin/sh

# Set user-limits for things such as max number of open file descriptors.
# The defaults for many of these settings are quite small.
src_user_limits_for_profile() {
  local nofileSoftLimit="${1:-8192}"
  local nofileHardLimit="${2:-200000}"

  if [ -n "$(command -v ulimit)" ]; then
    __set_user_max_open_file_descriptors \
      "${nofileSoftLimit}" "${nofileHardLimit}"
  fi
}

__set_user_max_open_file_descriptors() {
  local nofileSoftLimit="${1:-8192}"
  local nofileHardLimit="${2:-200000}"

  log_info "Setting max open file descriptor limits:" \
    "${YELLOW}soft${NC} = ${GREEN}${nofileSoftLimit}${NC}," \
    "${YELLOW}hard${NC} = ${GREEN}${nofileHardLimit}${NC}"
  ulimit -Sn "${nofileSoftLimit}"
  ulimit -Hn "${nofileHardLimit}"
}

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