#!/bin/sh

# Install the latest Marksman Markdown LSP server from GitHub.
install_marksman() {
  local marksmanVersion="$(curl -sL \
  "https://api.github.com/repos/artempyanykh/marksman/releases/latest" |
  grep -Po '"tag_name":\s*"\K[^"]*'
  )"
  local marksmanDest="${HOME}/.local/bin"
  local osType="$(getostype)"
  local marksmanOsDownloadType=""

  case "${osType}" in
    "Linux")
      marksmanOsDownloadType="linux"
      ;;
    "Darwin")
      marksmanOsDownloadType="macos"
      ;;
    *)
      err "Unknown OS type for Marksman download: ${MAGENTA}${osType}${NC}"
      return 1
      ;;
  esac

  (
    log_info "Downloading latest marksman version" && \
    curl -sL -o 'marksman' "https://github.com/artempyanykh/marksman/releases/download/${marksmanVersion}/marksman-${marksmanOsDownloadType}" && \
    chmod +x marksman && \
    mv marksman "${marksmanDest}/"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1