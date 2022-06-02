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

# macOS uses BSD versions of UNIX command line tools by default. The GNU
# versions are much more powerful and up-to-date, so we want to use them
# instead. The problem is, the GNU CLI tools are all prefixed with "g" on macOS.
# The non-prefixed versions are available, but not used by default. Thus, we
# need to add these non-prefixed versions of the tools to our PATH, and give
# them priority over the BSD tools.
create_gnu_cli_tool_aliases_for_mac() {
  local brewPrefix="$(brew --prefix)"
  local pathFile="${PATH_FILE:-${HOME}/.path}"

  log_info 'Adding GNU CLI tools to $PATH'
  log_debug "Updating locate database"

  # Update the locate database
  if [ -x "$(command -v gupdatedb)" ]; then
    sudo gupdatedb
  elif [ -x "/usr/libexec/locate.updatedb" ]; then
    sudo /usr/libexec/locate.updatedb
  fi

  log_debug "Writing gnubin directories to PATH file"
  glocate --regex "^${brewPrefix}.*gnubin\$" >>"${pathFile}"
  spath
  rmduplines "${pathFile}"
  export_path
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

# FRIG! macOS actually has the nerve to make their 'gcc' executable in /usr/bin
# a pointer script to Clang! I am not making this up. So, what are we then to do
# if we, ya know, want to use 'gcc' when we type 'gcc' in the terminal? I'll
# tell you what we do. Aliases! Homebrew installs 'gcc-<version specifier>'
# instead of just 'gcc' under /usr/local/bin. I don't know why, but I guess
# that's just what it does. Same with 'g++' for compiling C++ projects. This
# function creates those aliases.
alias_homebrew_gcc_executables() {
  local gccBaseDir="/usr/local/bin"
  local gccExec="$(find "${gccBaseDir}" -maxdepth 1 -mindepth 1 \
    -regex '.*/gcc-[0-9]\(\.?[0-9]\)+$')"
  local gppExec="$(find "${gccBaseDir}" -maxdepth 1 -mindepth 1 \
    -regex '.*/g\+\+-[0-9]\(\.?[0-9]\)+$')"

  if [ -x "${gccExec}" ]; then
    log_debug "Aliasing 'gcc' and 'cc' to: ${GREEN}${gccExec}${NC}"
    alias gcc="${gccExec}"
    alias cc="${gccExec}"
  else
    warn "Could not find 'gcc' executable under ${gccBaseDir}"
  fi

  if [ -x "${gppExec}" ]; then
    log_debug "Aliasing 'g++' and 'c++' to: ${GREEN}${gppExec}${NC}"
    alias g++="${gppExec}"
    alias c++="${gppExec}"
  else
    warn "Could not find 'g++' executable under ${gccBaseDir}"
  fi
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

# vim:foldenable:foldmethod=indent:foldlevel=0:foldnestmax=1
