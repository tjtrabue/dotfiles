#!/bin/sh

# Mac operating systems need the user to install a suite of developer tools
# before any useful software development can be performed.
install_mac_developer_tools() {
  # Make sure developer tools are installed
  xcode-select --install >>/dev/null 2>&1 || :
}

# Install the homebrew package manager for macOS.
install_homebrew() {
  if [ ! -x "$(command -v brew)" ]; then
    /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

# Ensure that homebrew knows where to find extra recipes.
tap_brew_casks() {
  log_info "Tapping homebrew casks"
  brew tap d12frosted/emacs-plus
}

# Install all homebrew packages
install_mac_packages() {
  local package

  tap_brew_casks

  if [ ! -f "${BREW_PACKAGES_FILE}" ]; then
    err "No homebrew packages file found at: ${BREW_PACKAGES_FILE}"
    return 1
  fi

  if [ -x "$(command -v gxargs)" ]; then
    # Use GNU xargs if possible
    gxargs brew install <"${BREW_PACKAGES_FILE}"
  else
    xargs brew install <"${BREW_PACKAGES_FILE}"
  fi

  # This one is a little harder to install, so I made it its own function.
  install_emacs_plus_for_macos
}

# Install the most important GNU tools on macOS.
install_gnu_cli_tools_for_mac() {
  brew install coreutils
  brew install binutils
  brew install diffutils
  brew install ed
  brew install findutils
  brew install gawk
  brew install global
  brew install gnu-getopt
  brew install gnu-indent
  brew install gnu-sed
  brew install gnu-tar
  brew install gnu-which
  brew install gnutls
  brew install grep
  brew install gzip
  brew install screen
  brew install tmux
  brew install watch
  brew install wdiff
  brew install wget
}

# Clone and import iTerm color schemes.
install_iterm_themes() {
  local repoUrl="https://github.com/mbadolato/iTerm2-Color-Schemes.git"
  local installDir="/tmp/$(basename "${repoUrl%.git}")"
  local importScript="${installDir}/tools/import-scheme.sh"

  log_info "Installing iTerm color schemes"

  if isgitrepo "${installDir}"; then
    log_info "Updating existing iTerm color schemes repository"
    git -C "${installDir}" reset --hard
    git -C "${installDir}" pull
  else
    git clone "${repoUrl}" "${installDir}"
  fi

  if ! isgitrepo "${installDir}"; then
    err "iTerm color schemes repo was not cloned correctly"
    return 1
  fi

  # Make sure script is executable
  chmod 755 "${importScript}"

  # Import all color schemes
  "${importScript}" "${installDir}/schemes"/* &&
    command cat <<EOF
iTerm color schemes successfully imported! Restart iTerm now, then check:

  Preferences > Profiles > Colors > Color Presets...

in order to setup your profile with the color scheme of your choice.
EOF
}

# Special install function for the emacs-plus package, which is Emacs that
# supports modern features, such as native code compilation, dbus support, cool
# icons, and more.
install_emacs_plus_for_macos() {
  local emacsPlusVersion="29"
  local icon="dragon-icon"

  PKG_CONFIG_PATH="$(brew --prefix)/lib/pkgconfig" CC=gcc CPPFLAGS="-L$(brew --prefix)/lib/gcc/current -lgccjit" brew install "emacs-plus@${emacsPlusVersion}" \
    --with-xwidgets \
    --with-mailutils \
    --with-dbus \
    --with-native-comp \
    --with-debug \
    "--with-${icon}"
}

# Delete and re-compile emacs-plus from source.
upgrade_emacs_plus_for_macos() {
  local response
  while ! echo "${response}" | grep -q "[YyNn]"; do
    command cat <<EOF
WARNING: Upgrading emacs-plus involves deleting your current installation,
cloning the updated repository, and recompiling emacs-plus from source. This is
a risky and error-prone process. You should backup any Emacs files you are
currently working on before proceeding. Do you wish to continue? [y/n]
EOF
    read -r response
  done

  if echo "${response}" | grep -q "[Nn]"; then
    return 0
  fi

  brew uninstall emacs-plus
  install_emacs_plus_for_macos
}

# This is a GUI-enabled, feature-rich, nightly build of Emacs, and the most
# dependable bleeding-edge version of Emacs for macOS that I am aware of.
# It is much easier to install than emacs-plus.
install_emacs_app_nightly_for_macos() {
  brew tap jimeh/emacs-builds
  brew install --cask emacs-app-nightly
}

# Enables hidden files in Finder by default.
show_hidden_files_in_finder() {
  defaults write com.apple.Finder AppleShowAllFiles true
  killall Finder
}

# Clean macOS system caches.
cleanup_mac() {
  local userCacheDir="${HOME}/Library/Caches"

  if [ -x "$(command -v brew)" ]; then
    log_info "Deleting Homebrew caches"
    brew cleanup --prune=all
  fi

  if [ -x "$(command -v xcrun)" ]; then
    log_info "Deleting old simlator devices"
    xcrun simctl delete unavailable
  fi

  if [ -d "${userCacheDir}" ]; then
    log_info "Purging user caches in: ${BLUE}${userCacheDir}${NC}"
    rm -rf "${userCacheDir}"/*
  fi

  command cat <<EOF
Done cleaning mac! You should restart your computer to ensure that all changes
take effect.
EOF
}

# Clean Xcode caches. Make sure to stop Xcode before running this function to
# ensure proper execution.
clean_xcode() {
  local xcodeCacheDir="${HOME}/Library/Caches/com.apple.dt.Xcode"
  local derivedDataDir="${HOME}/Library/Developer/Xcode/DerivedData"
  # Delete all subdirectories but do NOT delete this directory itself!!!
  local iosDeviceSupportDir="${HOME}/Library/Developer/Xcode/iOS DeviceSupport"
  local d

  log_info "Cleaning Xcode cache"

  for d in "${xcodeCacheDir}" "${derivedDataDir}"; do
    if [ -d "${d}" ]; then
      log_info "Removing Xcode cache dir: ${BLUE}${d}${NC}"
      rm -rf "${d}"
    fi
  done

  for d in "${iosDeviceSupportDir}"/*; do
    if [ -d "${d}" ]; then
      log_info "Removing iOS device support cache: ${BLUE}${d}${NC}"
      rm -rf "${d}"
    fi
  done

  command cat <<EOF
Done cleaning Xcode caches! Restart Xcode and follow these instructions:

- Clean the build folder
- Reset package caches
EOF
}

# Export the PATH variable specifically for building Homebrew or macOS
# executables that could not otherwise be built using the standard PATH. This
# can happen if PATH includes GNU utilities that conflict with the assumed
# macOS version of those tools (i.e., libtool).
#
# You can undo this operation by running `spath` in the terminal.
use_homebrew_path() {
  export PATH="$(cat <<EOF | tr '\n' ':' | sed 's/:$//'
/bin
/sbin
/usr/bin
/usr/bin/core_perl
/usr/bin/vendor_perl
/usr/sbin
/usr/local/bin
/usr/local/sbin
$(brew --prefix)/bin
EOF
  )"
}

# vim:foldenable:foldmethod=indent:foldlevel=0:foldnestmax=1
