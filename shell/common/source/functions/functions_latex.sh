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

  log_info "Installing digestif wrapper script to: ${BLUE}${digestifPath}${NC}"
  mkdir -p "${destDir}"
  curl -sL -o "${digestifPath}" -- "${digestifWrapperUrl}"
  chmod +x "${digestifPath}"

  # Run the downloaded wrapper script to finish the installation.
  # You'll have to quit out of this interactive process manually with C-c.
  log_info "Finishing digestif installation"
  digestif
}

# vim:foldenable:foldmethod=indent:foldnestmax=1