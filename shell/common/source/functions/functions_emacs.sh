#!/bin/sh

emacs_rm_backups() {
  find -L "${DOTFILES_HOME}" -type f -regextype posix-extended \
    -regex ".*(~)|(.*#.*)$" -delete
}

# Completely clean the ~/.emacs.d/ directory to prepare it for a reset.
emacs_wipe_config_dir() {
  local emacsHome="${HOME}/.emacs.d"

  rm -rf "${emacsHome}/auto-save-list" \
    "${emacsHome}/backups" \
    "${emacsHome}/el-get" \
    "${emacsHome}/eln-cache" \
    "${emacsHome}/persp-confs" \
    "${emacsHome}/semanticdb" \
    "${emacsHome}/transient" \
    "${emacsHome}/straight/build" \
    "${emacsHome}/straight/repos" \
    ;

  rm -f "${emacsHome}/company-statistics-cache.el" \
    "${emacsHome}/ede-projects.el" \
    "${emacsHome}/forge-database.sqlite" \
    "${emacsHome}/places" \
    "${emacsHome}/projectile-bookmarks.eld" \
    "${emacsHome}/recentf" \
    "${emacsHome}/srecode-map.el" \
    "${emacsHome}/straight/build-cache.el" \
    ;
}

# Start Emacs in Gnus mode to read email/news.
gnus() {
  emacs -f "gnus" &
}

# Download the fancy info+.el Emacs package from the GitHub mirror and stick it
# in our ~/.emacs.d/lisp/ directory.
install_info_plus() {
  local emacsLispDir="${EMACS_CONFIG_HOME}/lisp"
  local infoPlusGitUrl="https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/info%2B.el"

  mkdir -p "$emacsLispDir"
  curl -sL "$infoPlusGitUrl" >"${emacsLispDir}/info+.el"
}

# Start eshell by itslef in current terminal window
eshell() {
  emacs -Q -q -nw -f eshell
}

# Start emacs daemon (server) for emacsclient to connect to later.
emacsdaemon() {
  emacs --daemon
}

# Start GUI emacs client in backgroun by connecting to running emacs server.
eclient() {
  emacsclient -create-frame --alternate-editor="" &
}

# Shut down the Emacs server
shutdown_emacsdaemon() {
  emacsclient -e "(kill-emacs)"
}

# Install the native-comp feature branch of Emacs on various systems.
# This version of Emacs is much more performant than the standard version.
install_gccemacs() {
  local os="$(getdistro)"

  case "${os}" in
    "Darwin")
      __install_gccemacs_mac
      ;;
    *Linux*)
      __install_gccemacs_linux__from_source
      ;;
    *)
      err "Cannot install gccemacs for OS type: ${MAGENTA}${os}${NC}"
      return 1
      ;;
  esac
}

# Install the native-comp feature branch of Emacs on various systems.
# This version of Emacs is much more performant than the standard version.
__install_gccemacs_mac() {
  local emacsPlusVersion="${EMACS_PLUS_VERSION}"
  local emacsPlusPackageName="emacs-plus@${emacsPlusVersion}"
  local brewInstallDir="$(brew --prefix)/opt"
  local emacsPlusInstallDir="${brewInstallDir}/${emacsPlusPackageName}"

  # Make sure the emacs-plus version environment variable has been set to an
  # appropriate value.
  case "${emacsPlusVersion}" in
    '' | *[!0-9]*)
      err "EMACS_PLUS_VERSION environment variable not set to a number."
      return 1
      ;;
  esac

  log_info "Installing emacs-plus package"

  tap_brew_casks

  brew install "${emacsPlusPackageName}" \
    --with-dbus \
    --with-debug \
    --with-mailutils \
    --with-modern-purple-flat-icon \
    --with-native-comp \
    --with-xwidgets &&
  ln -s "${emacsPlusInstallDir}/Emacs.app" "/Applications/"
}

__install_gccemacs_linux__from_source() {
  local emacsGitUrl="git://git.savannah.gnu.org/emacs.git"
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDestDir="${emacsInstallParentDir}/$(basename "${emacsGitUrl}%.git")"

  __clone_or_update_emacs_source_repo
  __run_autogen_for_emacs "${emacsDestDir}"
  __configure_gccemacs_linux "${emacsDestDir}"
  __build_gccemacs_linux "${emacsDestDir}"
  __generate_gccemacs_documentation_linux "${emacsDestDir}"
  __install_compiled_gccemacs_linux "${emacsDestDir}"
  __install_gccemacs_documentation_linux "${emacsDestDir}"
}

# Customize gccemacs with optional features before compiling.
__configure_gccemacs_linux() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"
  local configurationOptions=(
    "--prefix=/usr"
    "--libexecdir=/usr/lib"
    "--sysconfdir=/etc"
    "--localstatedir=/var"
    "--enable-autodepend"
    "--enable-link-time-optimization"
    "--with-gameuser=:games"
    "--with-x-toolkit=gtk3"
    "--with-imagemagick"
    "--with-json"
    "--with-sound=alsa"
    "--with-modules"
    "--with-native-compilation"
    "--with-xwidgets"
    "--with-x"
    "--with-pgtk"
    "--without-xaw3d"
    "--without-gconf"
    "--without-gsettings"
  )

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    log_info "Configuring gccemacs build" &&
    cd "${emacsDir}" &&
    ./configure "${configurationOptions[@]}"
  )
}

# Compile gccemacs from source
__build_gccemacs_linux() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    log_info "Building gccemacs from source" &&
    cd "${emacsDir}" &&
    make -j"$(nproc)"
  )

  (
    log_info "Creating autoloads" &&
    cd "${emacsDir}/lisp" &&
    make autoloads
  )

  (
    log_info "Generating documentation" &&
    cd "${emacsDir}" &&
    make html &&
    make pdf
  )
}

__generate_gccemacs_documentation_linux() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    log_info "Generating documentation" &&
    cd "${emacsDir}" &&
    make html &&
    make pdf
  )
}

# We only need to run ./autogen.sh once. If we run it again, it breaks
# incremental compilation.
__run_autogen_for_emacs() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    cd "${emacsDir}"
    [ -x configure ] || (./autogen.sh git && ./autogen.sh autoconf)
  )
}

__install_compiled_gccemacs_linux() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    log_info "Installing Emacs" &&
    cd "${emacsDir}" &&
    sudo make install
  )

  (
    log_info "Installing Emacs documentation" &&
    cd "${emacsDir}" &&
    sudo make install-html &
    sudo make install-pdf
  )
}

__install_gccemacs_documentation_linux() {
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDir="${1:-${emacsInstallParentDir}/emacs}"

  if [ ! -d "${emacsDir}" ]; then
    err "Emacs directory not found at: ${BLUE}${emacsDir}${NC}"
    return 1
  fi

  (
    log_info "Installing Emacs documentation" &&
    cd "${emacsDir}" &&
    sudo make install-html &
    sudo make install-pdf
  )
}

__clone_or_update_emacs_source_repo() {
  local emacsGitUrl="git://git.savannah.gnu.org/emacs.git"
  local emacsInstallParentDir="${WS:-${HOME}/workspace}"
  local emacsDestDir="${emacsInstallParentDir}/$(basename "${emacsGitUrl%.git}")"

  if [ -d "${emacsDestDir}" ]; then
    log_info "Updating Emacs source dir"
    git -C "${emacsDestDir}" reset --hard
    git -C "${emacsDestDir}" clean -fd
    git -C "${emacsDestDir}" pull
  else
    log_info "Cloning Emacs Git repository to: ${BLUE}${emacsDestDir}${NC}"
    git clone "${emacsGitUrl}" "${emacsDestDir}"
  fi
}

# Checkout the configured default branch for each repository cloned by the
# straight.el package manager and update each repo. This is useful for when one
# package that you desire declares another package you want to use as a
# vdependency, and checks out a specific revision of the second package that
# messes up your Emacs configuration.
straight_update_repos() {
  local emacsHome="${EMACS_CONFIG_HOME:-${HOME}/.emacs.d}"
  local straightHome="${emacsHome}/straight"
  local straightRepos="${straightHome}/repos"
  local d

  if [ ! -d "${straightRepos}" ]; then
    err "Directory ${straightRepos} does not exist."
    return 1
  fi

  log_info "Updating default branches for all straight.el cloned repositories"
  for d in "${straightRepos}"/*; do
    __update_straight_repo "${d}"
  done
}

# Update a repository cloned by straight.el.
__update_straight_repo() {
  local repo="${1}"
  local numRetries=3
  local repoUpdated=0
  local i
  local defaultBranch

  if [ ! -d "${repo}" ]; then
    warn "${BLUE}${repo}${NC} is not a directory"
    return 1
  fi

  log_info "Updating repository: ${BLUE}$(basename "${repo}")${NC}"

  # Checkout the default branch for the repository if we are in a detached HEAD
  # state. We don't want to always check out the default branch because we may
  # have specified a different branch in our straight.el rescipe for this
  # repository. However, a detached HEAD state indicates that the repository was
  # cloned as a dependency of another package, and we do want to make sure it
  # gets updated in that case.
  if ! git -C "${repo}" symbolic-ref -q HEAD >>/dev/null 2>&1; then
    defaultBranch="$(defaultbranch "${repo}")"
    log_info "Checking out default branch: ${GREEN}${defaultBranch}${NC}"
    # Reset all changes to make for a clean working tree
    totalgitreset -f "${repo}"
    # Checkout the default branch
    git -C "${repo}" checkout -f "${defaultBranch}"
  fi

  # Try a few times to update the repository.
  for i in {1..${numRetries}}; do
    if git -C "${repo}" pull; then
      repoUpdated=1
      break
    else
      warn "Did not update repo: ${BLUE}$(basename "${repo}")${NC} on attempt" \
        "${i} of ${numRetries}"
    fi
  done

  # Print error message if all update attempts failed.
  if [ "${repoUpdated}" -eq 0 ]; then
    err "Could not update repo: ${BLUE}$(basename "${repo}")${NC}." \
      "Try to update it manually."
  fi
}

# Clone my personal roam-notes database.
clone_roam_notes() {
  local emacsHome="${EMACS_HOME:-${HOME}/.emacs.d}"
  local roamNotesHome="${emacsHome}/roam-notes"
  local roamNotesRepoUrl="git@github.com:tjtrabue/roam-notes.git"
  local response=""

  # Make sure the ~/.emacs.d directory is present before proceeding.
  mkdir -p "${emacsHome}"

  if [ -d "${roamNotesHome}" ]; then
    echoe "WARNIG: Roam notes directory exists at: ${BLUE}${roamNotesHome}${NC}." \
      "Delete it and re-clone the repository? [y/n]"

    while echo "${response}" | grep -vq "^[YyNn]$"; do
      read -r response
    done

    if [ "${response}" = "y" ] || [ "${response}" = "Y" ]; then
      rm -rf "${roamNotesHome}"
    else
      err "Cancelled by user."
      return 1
    fi
  fi

  log_info "Cloning ${GREEN}roam-notes${NC} repository..."
  git clone "${roamNotesRepoUrl}" "${roamNotesHome}"
}

# Modeline for this file (leave it commented!)
# vim:foldenable:foldmethod=syntax:foldlevel=0:foldnestmax=1
