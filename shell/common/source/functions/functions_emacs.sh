#!/bin/sh

emacs_rm_backups() {
  find -L "$DOTFILES_HOME" -type f -regextype posix-extended \
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
  local emacsLispDir="$EMACS_CONFIG_HOME/lisp"
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

# Checkout the configured default branch for each repository cloned by the
# straight.el package manager and update each repo. This is useful for when one
# package that you desire declares another package you want to use as a
# vdependency, and checks out a specific revision of the second package that
# messes up your Emacs configuration.
straight_update_repos() {
  local emacsHome="${EMACS_CONFIG_HOME:-${HOME}/.emacs}"
  local straightHome="${emacsHome}/straight"
  local straightRepos="${straightHome}/repos"
  local numRetries=3
  local i
  local d
  local currBranch
  local defaultBranch
  local repoUpdated

  if [ ! -d "${straightRepos}" ]; then
    err "Directory ${straightRepos} does not exist."
    return 1
  fi

  log_info "Updating default branches for all straight.el cloned repositories"
  for d in "${straightRepos}"/*; do
    currBranch="$(git -C "${d}" rev-parse --abbrev-ref HEAD)"
    defaultBranch="$(defaultbranch "${d}")"
    repoUpdated=0

    log_info "In repository: ${BLUE}$(basename "${d}")${NC}"
    if [ "${currBranch}" != "${defaultBranch}" ]; then
      log_info "Checking out default branch: ${GREEN}${defaultBranch}${NC}"
      # Reset all changes to make for a clean working tree
      totalgitreset "${d}"
      # Checkout the default branch
      git -C "${d}" checkout -f "${defaultBranch}"
    fi

    # Try a few times to update the repository.
    for i in {1..${numRetries}}; do
      if git -C "${d}" pull; then
        repoUpdated=1
        break
      else
        warn "Did not update repo: ${BLUE}$(basename "${d}")${NC} on attempt" \
          "${i} of ${numRetries}"
      fi
    done

    # Print error message if all update attempts failed.
    if [ "${repoUpdated}" -eq 0 ]; then
      err "Could not update repo: ${BLUE}$(basename "${d}")${NC}." \
        "Try to update it manually."
    fi
  done
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
