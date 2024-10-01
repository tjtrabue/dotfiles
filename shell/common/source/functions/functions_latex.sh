#!/bin/sh

# Install the new-fangled digestif LSP server for LaTeX. It's quite cool!
#
# Note for Emacs users:
# If you use `lsp-mode' as your LSP client, you may need to restart Emacs a few
# times before lsp-mode picks up on the fact that digestif exists on your path.
install_digestif_lsp() {
  local digestifWrapperUrl="https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif"
  local prefix="${HOME}/.local"
  local destDir="${prefix}/bin"
  local digestifPath="${destDir}/digestif"

  if [ -x "${digestifPath}" ]; then
    warn "${CYAN}digestif${NC} wrapper already exists in expected location." \
      "Exiting."
    return 1
  fi

  log_info "Installing digestif wrapper script to: ${BLUE}${digestifPath}${NC}"
  mkdir -p "${destDir}"
  curl -sL -o "${digestifPath}" -- "${digestifWrapperUrl}"
  chmod +x "${digestifPath}"

  # Run the downloaded wrapper script to finish the installation.
  # You'll have to quit out of this interactive process manually with C-c.
  log_info "Finishing digestif installation"
  digestif
}

# Update all TexLive LaTeX packages.
# NOTE: This function assumes that you have installed TexLive manually, not
#       through your system's package manager!
update_texlive_packages() {
  # Add the '--reinstall-forcibly-removed' flag if you want to recover something
  # you uninstalled accidentally.
  log_info "Updating TexLive packages through CTAN"
  tlmgr update --self --all
}

# vim:foldenable:foldmethod=indent:foldnestmax=1