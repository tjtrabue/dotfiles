#!/bin/sh

emacs_rm_backups() {
  find -L "${DOTFILES_HOME}" -type f -regextype posix-extended \
    -regex ".*(~)|(.*#.*)$" -delete
}

# Get rid of cache directores under ~/.emacs.d/ to help get rid of unusual
# behavior.
emacs_rm_caches() {
  local emacsHome="${HOME}/.emacs.d"
  local straightHome="${emacsHome}/straight"

  log_info "Deleting Emacs cache directories"
  rm -f "${emacsHome:?}/persp-state.el"
  rm -rf "${emacsHome:?}/.cache"
  rm -rf "${emacsHome:?}/eln-cache"
  rm -rf "${emacsHome:?}/elisp-autofmt-cache"

  log_info "Deleting Straight.el byte-compiled files"
  if [ -n "$(command -v fd)" ]; then
    # `fd` is a faster, more modern replacement for `find`.
    fd -t f -x rm '{}' ';' -- ".*\.elc$" "${straightHome}"
  else
    find "${straightHome}" -type f -name '*.elc' -delete
  fi
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

# Checkout the configured default branch for each repository cloned by the
# straight.el package manager and update each repo. This is useful for when one
# package that you desire declares another package you want to use as a
# dependency, and checks out a specific revision of the second package that
# messes up your Emacs configuration.
straight_update_repos() {
  local emacsHome="${EMACS_CONFIG_HOME:-${HOME}/.emacs.d}"
  local straightHome="${emacsHome}/straight"
  local straightRepos="${straightHome}/repos"
  local fast=false
  local d
  local OPTIND
  local o

  while getopts ":f" o; do
    case "${o}" in
    f)
      # Fast update method does not do any analysis of the current branch of the
      # straight repo. It simply forces a git update of the current ref. This
      # approach is much faster than the default method.
      fast=true
      ;;
    *)
      err "Unknown operand"
      return 1
      ;;
    esac
  done
  shift $((OPTIND - 1))

  if [ ! -d "${straightRepos}" ]; then
    err "Directory ${straightRepos} does not exist."
    return 1
  fi

  log_info "Updating default branches for all straight.el cloned repositories"
  if "${fast}"; then
    __straight_update_repos_fast "${straightRepos}"
  else
    for d in "${straightRepos}"/*; do
      __update_straight_repo "${d}"
    done
  fi
}

__straight_update_repos_fast() {
  local straightRepos="${1:-${HOME}/.emacs.d/straight/repos}"

  if [ ! -d "${straightRepos}" ]; then
    err "Directory ${straightRepos} does not exist."
    return 1
  fi

  log_info "Using fast update method for straight.el repos"
  if [ -x "$(command -v parallel)" ]; then
    # Use GNU Parallel if available.
    find "${straightRepos}" -maxdepth 1 -mindepth 1 -type d |
      parallel 'printf "%s: ${BLUE}%s${NC}\n" "Updating repo" "{/}" &&
        git -C {} pull -f'
  else
    find "${straightRepos}" -maxdepth 1 -mindepth 1 -type d \
      -exec git -C '{}' pull -f \;
  fi
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
    git -C "${repo}" clean -fdx
    git -C "${repo}" reset --hard HEAD
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
        "${GREEN}${i}${NC} of ${GREEN}${numRetries}${NC}"
    fi
  done

  # Print error message if all update attempts failed.
  if [ "${repoUpdated}" -eq 0 ]; then
    err "Could not update repo: ${BLUE}$(basename "${repo}")${NC}." \
      "Try to update it manually."
  fi
}

# Remove a package installed with Straight.el.
straight_rm_repo() {
  local emacsHome="${EMACS_CONFIG_HOME:-${HOME}/.emacs.d}"
  local straightHome="${emacsHome}/straight"
  local straightRepos="${straightHome}/repos"
  local straightBuild="${straightHome}/build"
  local repo="$1"

  if [ -z "${repo}" ]; then
    err "No repo provided"
    return 1
  fi

  if [ ! -d "${straightRepos}/${repo}" ]; then
    err "Straight repo ${BLUE}${repo}${NC} does not exist"
    return 2
  fi

  if [ ! -d "${straightBuild}/${repo}" ]; then
    err "Straight build dir ${BLUE}${repo}${NC} does not exist"
    return 3
  fi

  log_info "Removing straight repo: ${BLUE}${repo}${NC}"

  rm -rf "${straightRepos}/${repo:?}"
  rm -rf "${straightBuild}/${repo:?}"
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

# Install the Cask package manager and build tool for Emacs.
# Cask is much like Maven for Emacs Lisp projects.
install_emacs_cask() {
  local caskGitUrl="https://github.com/cask/cask"
  local ws="${WS:-${HOME}/workspace}"
  local caskDest="${ws}/cask"

  clone_or_update_git_repo "${caskGitUrl}" "${caskDest}"
  (
    log_info "Installing Cask for Emacs" &&
      cd "${caskDest}" &&
      make install
  )
}

# Modeline for this file (leave it commented!)
# vim:foldenable:foldmethod=indent:foldlevel=0:foldnestmax=1
