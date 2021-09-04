#!/bin/sh

# Clean up all swap/backup/undo files for Neovim.
clean_nvim() {
  local nvimHome="${NVIM_HOME:-${HOME}/.config/nvim}"
  local d

  for d in "${nvimHome}"/{backups,swaps,undo}; do
    if [ -d "${d}" ]; then
      log_info "Cleaning up directory: ${BLUE}${d}${NC}"
      rm -f "${d}"/*
    fi
  done
}

# Installs the latest, greatest Neovim from the official site as an
# appimage, and links the included executable to /usr/local/bin.
install_neovim_nightly_appimage() {
  local neovimReleasesDir="${NVIM_HOME:-${HOME}/.config/nvim}/.releases"
  local neovimAppimagesDir="${neovimReleasesDir}/appimages"
  local latestAppimageDir="${neovimAppimagesDir}/latest"
  local appimageRunnable="${latestAppimageDir}/squashfs-root/usr/bin/nvim"
  local appimageName="nvim.appimage"
  local nvimSymlinkDest="/usr/local/bin"

  __download_neovim_nightly_appimage

  (
    cd "${latestAppimageDir}"
    chmod +x "${appimageName}"
    ./"${appimageName}" --appimage-extract
  )

  # Ask user before overwriting an existing /usr/local/bin/nvim executable.
  if [ -x "${nvimSymlinkDest}/nvim" ] &&
  ! __check_nvim_appimage_link_exists; then
    if ! __prompt_user_before_overwriting_nvim_link; then
      echoe "Exiting due to user request"
      return 1
    fi
  fi

  log_info "Linking new neovim executable to: ${nvimSymlinkDest}/nvim"
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

# Ask user before overwriting an existing /usr/local/bin/nvim executable.
__prompt_user_before_overwriting_nvim_link() {
  local response
  local usrLocalNvim="/usr/local/bin/nvim"

  while ! echo "$response" | grep -q '[YyNn]'; do
    echoe "Neovim executable exists at ${usrLocalNvim}. Overwrite this" \
      "executable? [yN]"
    read -r response
  done

  if echo "${response}" | grep -q '[Nn]'; then
    return 1
  fi
}

# Check if we already have a linked nvim executable in /usr/local/bin
__check_nvim_appimage_link_exists() {
  local neovimReleasesDir="${NVIM_HOME:-${HOME}/.config/nvim}/.releases"

  readlink /usr/local/bin/nvim |
  grep -q "${neovimReleasesDir}" >>/dev/null 2>&1
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
