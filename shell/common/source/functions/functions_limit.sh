#!/bin/sh

# It's often quite necessary to increase Linux's limit for open files per shell
# session, as the default is usually too small for modern applications.
set_max_no_open_files() {
  local maxNoOpenFiles="${1:-10240}"
  local sysctlConfFile="/etc/sysctl.conf"

  log_info "Setting the max number of open files to:" \
    "${GREEN}${maxNoOpenFiles}${NC}"
  sudo sysctl -w fs.file-max="${maxNoOpenFiles}"
  sudo echo -n "${maxNoOpenFiles}" >>"${sysctlConfFile}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1