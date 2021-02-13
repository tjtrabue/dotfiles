#!/bin/sh
# Contains custom Git functions that can be run in the terminal.

# General {{{
# Determines whether or not the current directory is in a git repository
isrepo() {
  git rev-parse --is-inside-work-tree &>/dev/null
  if [[ $? -eq 0 ]]; then
    echo true 1>&2
  else
    echo false 1>&2
  fi
}

# Runs the logic for generating a new SSH key for GitHub and saving the pair:
ssh-gen() {
  local key_name="auto generated ssh key for GitHub"
  if [[ "$1" == "-n" && "$2" != "" ]]; then
    key_name="$2"
  fi

  local response=""
  while [[ "$response" == "" ]]; do
    echo "Enter file name for key (dafault is ~/.ssh/id_rsa)" 1>&2
    read response
    response=${response:-~/.ssh/id_rsa}
  done

  ssh-keygen -t rsa -b 4096 -C "tom.trabue@gmail.com" | echo "$response" | echo "" | echo ""
  eval "$(ssh-agent -s)"
  ssh-add "$response"

  pbcopy <"$response.pub"
  curl -u "tjtrabue" --data "{\"title\":\"$key_name\", \"key\":\"$(pbpaste)\"}" "https://api.github.com/user/keys"
}

# Retrieves the git url for the current repository
remoteurl() {
  git remote -v | egrep '^origin.*push' | awk '{print $2}'
}

# Returns the main remote branch for the repository:
mainbranch() {
  git remote show origin | grep 'HEAD branch' | awk '{print $3}'
}

# Opens the commit message for the current repo in the configured editor:
emsg() {
  edit "$(git rev-parse --show-toplevel)/.git/COMMIT_EDITMSG"
}

# Checks out the commit for a given tag name and makes a branch for that tag:
gcht() {
  if [[ -z "$1" ]]; then
    err "Must provide a tag name!"
    return 1
  fi
  git checkout "tags/$1" -b "$1"
}
# }}}

# Cloning {{{
# Clones a repo and all submodules. Prompts user for username/email
clone() {
  if [[ -n "$1" ]]; then
    echoe "Enter username for this project:"
    read -r username
    echoe "Now enter the email address for the project:"
    read -r email

    git clone --recursive "$1"
    pushd $(basename "$1" | sed 's/\.git$//')
    git config user.name "$username"
    git config user.email "$email"
    popd
  else
    err "No url entered"
    return 1
  fi
}
# }}}

# Submodules {{{
# Lists all submodules in a repo
ls-submods() {
  $(git rev-parse --is-inside-work-tree) &>/dev/null
  if [[ $? -eq 0 ]]; then
    local repo_home="$(dirname $(git rev-parse --git-dir))"
    grep path $repo_home/.gitmodules | sed 's/.*= //'
  else
    err "Not in a git repository"
    return 1
  fi
}

# Creates a git submodule based on a git url.
# Alternately deletes a submodule in a repo:
submod() {
  if [[ "$1" == "-r" ]]; then
    if [[ -z "$2" ]]; then
      err "Must enter the path to the submodule"
      return 1
    fi

    local submods=($(ls_submods))
    for sub in "${submods[@]}"; do
      if [[ "$2" == "$sub" ]]; then
        echoe "Removing submodule $2"
        mv "$2" "$2_tmp"
        git submodule deinit "$2"
        git rm --cached "$2"
        mv "$2_tmp" "$2"
        rm -rf "$(git rev-parse --git-dir)/modules/$2"
      fi
    done
  else
    git submodule add "$1"
  fi
}

# Update all submodules in the current git repo (requires git version
# 1.6.1 or later)
upsubs() {
  if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git submodule foreach git pull origin master
  else
    err "Not in a git repository"
    return 1
  fi
}
# }}}

# Editor {{{
# Changes the designated git editor.
# Options are Sublime, Atom, and Vim:
swged() {
  if [[ "$#" -ne 1 ]]; then
    echoe "No editor name supplied."
    echo "Valid options are sublime, atom, and vim." 1>&2
    return 1
  fi

  case "${1}" in
  "sublime")
    git config --global core.editor "subl -w"
    ;;
  "atom")
    git config --global core.editor "atom --wait"
    ;;
  "vim")
    git config --global core.editor "vim"
    ;;
  *)
    echoe "Unknown operand $1."
    echo "Please enter sublime, atom, or vim as an" 1>&2
    echo "argument to this function." 1>&2
    return 2
    ;;
  esac
}
# }}}

# Reverting/resetting {{{
# Reverts the current repo to the state of its previous commit:
creset() {
  git rev-parse --is-inside-work-tree &>/dev/null
  if [[ $? -eq 0 ]]; then
    git reset --soft HEAD~1
    git reset HEAD "$(git rev-parse --show-toplevel)"
  else
    return 1
  fi
}

# Resets all uncommitted changes in the current repository
ucreset() {
  git rev-parse --is-inside-work-tree &>/dev/null
  if [[ $? -eq 0 ]]; then
    # Revert changes to modified files.
    git reset --hard

    # Remove all untracked files and directories. (`-d` is `remove directories`, `-ff` is `force`)
    git clean -d -ff
  else
    return 1
  fi
}
# }}}

# Committing {{{
gcm() {
  local message="$1"
  if [[ -z "$message" ]]; then
    while [[ -z "$message" ]]; do
      echoe "Enter commit message:"
      read -er message
    done
  fi
  git commit -m "$message"
}
# }}}

# Show last diff for a given file
lastchange() {
  local file="$1"

  if [ -z "$file" ]; then
    err "No file given to ${FUNCNAME[0]}"
    return 1
  fi

  if [ ! -f "$file" ]; then
    err "Path ${BLUE}$file${NC} is not a file"
    return 2
  fi

  git log -p -1 "$file"
}

# vim:foldenable:foldmethod=syntax
