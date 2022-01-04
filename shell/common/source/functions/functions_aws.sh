#!/bin/sh

# Install the AWS CLI tools for any supported operating system.
install_aws_cli() {
  local os="$(getdistro)"

  log_info "Installing AWS CLI"
  case "${os}" in
  "Darwin")
    __install_aws_cli_mac
    ;;
  *Linux*)
    __install_aws_cli_linux
    ;;
  *)
    err "Could not install AWS CLI for OS: ${GREEN}${os}${NC}"
    return 1
    ;;
  esac
}

__install_aws_cli_mac() {
  local awsCliPkg="AWSCLIV2.pkg"

  log_info "Installing AWS CLI for macOS"
  if [ -n "$(command -v brew)" ]; then
    log_info "Installing AWS CLI via Homebrew"
    brew install awscli
  else
    log_info "Installing AWS CLI via Amazon PKG"
    curl -o "${awsCliPkg}" "https://awscli.amazonaws.com/${awsCliPkg}" &&
      sudo installer -pkg "${awsCliPkg}" -target / &&
      rm -f "${awsCliPkg}"
  fi
}

__install_aws_cli_linux() {
  local installDir="${APPS:-${HOME}/applications}"
  local archiveDir="${ARCHIVES:-${installDir}/archives}"
  local awsZip="${archiveDir}/awscliv2.zip"

  log_info "Installing AWS CLI for Linux"
  log_info "Zip dir: ${BLUE}${archiveDir}${NC}"
  log_info "Install dir: ${BLUE}${installDir}${NC}"

  curl -fsSL --create-dirs -o "${awsZip}" \
    "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip"

  unzip -d "${installDir}" "${awsZip}"
  sudo "${installDir}"/aws/install
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
