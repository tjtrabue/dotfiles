#!/bin/sh

install_neovim_nightly_appimage() {
  local neovimReleasesDir="${NVIM_HOME:-${HOME}/.config/nvim}/.releases"
  local neovimAppimagesDir="${neovimReleasesDir}/appimages"
  local latestAppimageDir="${neovimAppimagesDir}/latest"
  local downloadUrl="https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage"
  local appimageName="$(basename "${downloadUrl}")"
  local latestAppimage="${latestAppimageDir}/${appimageName}"
  local appimageRunnable="${latestAppimageDir}/squashfs-root/usr/bin/nvim"
  local nvimSymlinkDest="/usr/local/bin"

  __download_neovim_nightly_appimage

  (
    cd "${latestAppimageDir}"
    chmod +x "${appimageName}"
    ./"${appimageName}" --appimage-extract
  )

  log_info "Linking new neovim executable to: ${nvimSymlinkDest}"
  sudo ln -sf -t "${nvimSymlinkDest}" "${appimageRunnable}" >>/dev/null 2>&1
}

__download_neovim_nightly_appimage() {
  local neovimReleasesDir="${NVIM_HOME:-${HOME}/.config/nvim}/.releases"
  local neovimAppimagesDir="${neovimReleasesDir}/appimages"
  local latestAppimageDir="${neovimAppimagesDir}/latest"
  local downloadUrl="https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage"
  local appimageName="$(basename "${downloadUrl}")"
  local latestAppimage="${latestAppimageDir}/${appimageName}"

  # Delete last latest image.
  # Notice how we protext this command from removing /* in case the
  # variable is not set.
  rm -rf "${latestAppimageDir:?'No latest appimage dir'}"/*

  # Make directories
  mkdir -p "${latestAppimageDir}"

  log_info "Downloading latest Neovim appimage..."
  curl -L -o "${latestAppimage}" "${downloadUrl}"
}

__check_nvim_appimage_link_exists() {
  local neovimReleasesDir="${NVIM_HOME:-${HOME}/.config/nvim}/.releases"

  readlink /usr/local/bin/nvim \
    | grep -q "${neovimReleasesDir}" >>/dev/null 2>&1
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
