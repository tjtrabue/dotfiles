#!/bin/sh

# Install the terraform language server.
install_terraform_language_server() {
  local repoUrl="https://github.com/juliosueiras/terraform-lsp.git"
  local repoDir="${WS:-${HOME}/workspace}/$(basename "${repoUrl%.git}")"
  local installPrefix="${HOME}"
  local installDest="${installPrefix}/bin"

  log_info "Installing Terraform LSP"

  if [ ! -x "$(command -v go)" ]; then
    err "go CLI command not found on PATH"
    return 1
  fi

  if [ ! -d "${repoDir}" ]; then
    git clone "${repoUrl}" "${repoDir}"
  else
    git -C "${repoDir}" checkout "$(defaultbranch "${repoDir}")" 2>&1 >>/dev/null
    git -C "${repoDir}" pull
  fi

  (
    cd "${repoDir}"
    # Download the modules for the project
    GO111MODULE=on go mod download
    # Build the project. Alternatively run "go build"
    make
    # Install the project
    make copy DST="${installDest}"
  )
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
