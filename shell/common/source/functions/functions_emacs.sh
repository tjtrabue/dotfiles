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

# Checkout master or standard branch for all repos cloned by straight.el if the
# repos are in a "detached HEAD" state. Regardless of whether or not the repo's
# branch changed, update the mainline branch for the repository.
straight_update_repos() {
  local straightHome="${HOME}/.emacs.d/straight"
  local straightRepos="${straightHome}/repos"
  local defaultBranch
  local repo
  local d

  log_info "Updating all straight.el cloned repositories"
  for d in "${straightRepos}"/*; do
    if isrepo "${d}"; then
      # If d is a git repo...
      repo="$(basename "${d}")"
      log_info "Current repo: ${BLUE}${repo}${NC}"
      if [ "$(git -C "${d}" rev-parse --abbrev-ref --symbolic-full-name HEAD)" \
        = "HEAD" ]; then
        # If we are in detached HEAD, switch back to the default branch. We only
        # care about detached HEAD state because sometimes we want to remain
        # on a branch other than the default branch, such as 'develop', which we
        # specified in our straight.el recipe.
        defaultBranch="$(defaultbranch "${d}")"

        if ! git -C "${d}" rev-parse --verify "${defaultBranch}" \
          >>/dev/null 2>&1; then
          err "Could not determine default branch for repository:" \
            "${BLUE}${repo}${NC}"
          continue
        fi

        log_info "Default branch: ${MAGENTA}${defaultBranch}${NC}"

        # Checkout the specified branch if we are in detached HEAD state.
        log_info "Switching to ${GREEN}${defaultBranch}${NC} branch"
        git -C "${d}" checkout -f "${defaultBranch}"
      fi
      # Update the repo regardless of which branch it was previously on.
      git -C "${d}" pull
    fi
  done
}

# Modeline for this file (leave it commented!)
# vim:foldenable:foldmethod=syntax:foldnestmax=1
