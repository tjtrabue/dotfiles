#!/bin/sh

# Link the Vim configuration directory found in the dotfiles repository to
# ~/.vim to make them available to the user.
link_vim_config_dir() {
  local vimDotfilesConfigDir="${DOTFILES_HOME}/link/vim"
  local vimConfigDir="${HOME}/.vim"
  local vimConfigBackup="${vimConfigDir}.bak"

  if [ -d "${vimConfigDir}" ]; then
    log_info "Backing up old Vim config files to: ${vimConfigBackup}"
    mv "${vimConfigDir}" "${vimConfigBackup}"
  fi

  log_info "Linking dotfiles Vim configuration directory"
  ln -sfb "${vimDotfilesConfigDir}" "${vimConfigDir}"

  if [ -d "${vimConfigBackup}" ]; then
    log_info "Copying backed up Vim config files to: ${vimConfigDir}"
    # If we backed up old Vim config files, copy them back over after linking:
    rsync -avh "${vimConfigBackup}/" "${vimConfigDir}"
    # Finally, remove the backup directory:
    rm -rf "${vimConfigBackup}"
  fi
}

# Install all extensions for coc.nvim.
# NOTE: You must have yarn installed to install coc extensions.
install_coc_extensions() {
  local cocHome="${HOME}/.config/coc"
  local cocExtDir="${cocHome}/extensions"

  if [ -d "${cocExtDir}" ]; then
    log_info "Installing coc.nvim extensions"
    (
      cd "${cocExtDir}"
      if [ ! -x "$(command -v yarn)" ]; then
        # Make sure yarn is installed and ready to go.
        install_node_packags
      fi
      yarn install --ignore-engines --ignore-platform
    )
    log_info "Done!"
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
