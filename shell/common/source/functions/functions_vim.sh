#!/bin/sh

# Install all extensions for coc.nvim.
# NOTE: You must have yarn installed to install coc extensions.
install_coc_extensions() {
  local cocHome="${HOME}/.config/coc"
  local cocExtDir="${cocHome}/extensions"

  if [ -d "${cocExtDir}" ]; then
    log_info "Installing coc.nvim extensions"
    (
      cd "${cocExtDir}"
      yarn install --ignore-engines --ignore-platform
    )
    log_info "Done!"
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
