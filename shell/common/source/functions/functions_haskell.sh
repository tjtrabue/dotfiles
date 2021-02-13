#!/bin/sh

install_haskell_packages() {
  local packageFile="$DOTFILES_PACKAGES_DIR/haskell_packages.txt"

  log_info "Install Haskell packages from Hackage"
  # Update Hackage repo manifest
  cabal update
  eval "cabal install" "--overwrite-policy=always" \
    "$(tr '\n' ' ' <"$packageFile")"
}

purge_cabal() {
  local cabalHome="${HOME}/.cabal"
  local ghcConfHome="${HOME}/.ghc"
  local response=""

  while echo "$response" | grep -v '[NnYy]'; do
    echoe "Do you want to remove all Cabal metadata?"
    echoe "This will remove all of your Hackage packages installed with Cabal" \
      "[y/n]"
    read -sn1 response
  done
  if [ "$response" == 'n' ] || [ "$resopnse" == 'N' ]; then
    warn "User aborted process"
    return 1
  fi

  log_info "Removing all cabal metadata"
  rm -rf "$(find "$ghcConfHome" -maxdepth 1 -type d)"
  rm -rf "${cabalHome:?}/lib"
  rm -rf "${cabalHome:?}/packages"
  rm -rf "${cabalHome:?}/share"

  log_info "Updating cabal manifest"
  cabal update
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
