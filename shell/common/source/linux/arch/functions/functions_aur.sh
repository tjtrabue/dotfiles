#!/bin/sh

# Automatic AUR interaction.
#
# This is a thin, flimsy convenience function that should only be used to
# install a real AUR helper, such as 'paru' or 'aura'. They handly installation,
# upgrading, downgrading, and dependency resolution MUCH better than this
# function.
aur() {
  local aurCommand
  local options

  _aur_help() {
    cat <<EOF
${FUNCNAME[1]} - Manage packages from the Arch User Repositories \(AUR\)

'aur' is a CLI tool for interacting with the AUR. You may use 'aur' to  install,
update, and remove packages from the AUR automatically.

USAGE
  ${FUNCNAME[1]} [-h|--help]
                 [get|install|update|remove|uninstall] package [package ...]

COMMANDS
  get [PACKAGES]
    Downloads package repositories from the AUR but does not build or install
    them.

  install [PACKAGES]
    Installs one or more packages from the AUR to ${AUR_HOME}.

  update [PACKAGES]
    Updates one or more AUR packages installed to ${AUR_HOME}.

  remove [PACKAGES]
  uninstall [PACKAGES]
    Removes one or more AUR packages installed to ${AUR_HOME}.
EOF
  }

  options="$(getopt -o h -l help -- "$@")"
  eval set -- "$options"
  while true; do
    case "$1" in
      -h | --help)
        _aur_help
        return 0
        ;;

      --)
        shift
        break
        ;;

      *)
        err "Unknown argument $1 to ${FUNCNAME[0]}"
        return 2
        ;;
    esac
  done

  _prepare_aur_environment
  aurCommand="$1"
  shift
  case "$aurCommand" in
    "get")
      aur-get "$@"
      ;;
    "install")
      aur-get "$@" &&
      aur-install "$@"
      ;;
    "update")
      aur-update "$@"
      ;;
    "remove" | "uninstall")
      aur-remove "$@"
      ;;
    *)
      err "Unknown AUR command $aurCommand. Should be one of: install, update, remove."
      return 1
      ;;
  esac
}

# Download an AUR package but do not build/install
aur-get() {
  # Get args from stdin if none are passed as args
  set -- ${*:-$(</dev/stdin)}
  echoe "Have $# requests: $*"

  local -a newPackages=($(_get_list_of_new_packages "$@"))
  local aurBaseUrl="https://aur.archlinux.org"
  local package
  local newPackageCount

  _check_aur_provided "$@" || return 1

  [ ! -d "$AUR_HOME" ] && mkdir -p "$AUR_HOME"

  newPackageCount="${#newPackages[@]}"
  if [ "$newPackageCount" -gt 0 ]; then
    log_info "Downloading $newPackageCount new repositories..."
    # Download repositories in parallel
    if parallel --bar git clone "${aurBaseUrl}/{}.git" "${AUR_HOME}/{}" \
      ::: "${newPackages[@]}"; then
      for package in "${newPackages[@]}"; do
        if [ -z "$(ls "$AUR_HOME/$package")" ]; then
          warn "Removing empty repo '$package'"
          rm -rf "${AUR_HOME:?}/$package"
        fi
      done
      _print_final_effect_msg "Got AUR packages" "${newPackages[@]}"
    else
      err "Could not download all repositories"
      return 2
    fi

  else
    echoe ""
    succ "Nothing to do"
  fi
}

# Installs one or more Arch Linux packages from the AUR.
aur-install() {
  # Get args from stdin if none are passed as args
  set -- ${*:-$(</dev/stdin)}

  local -a aurPackages=("$@")
  local package

  _check_aur_provided "${aurPackages[@]}" || return 1

  # Build packages one at a time, since pacman can only handle one build at once.
  log_info "Building packages..."
  for package in "${aurPackages[@]}"; do
    (
      cd "${AUR_HOME}/$package"
      if _prepare_and_build_package; then
        rm -rf "${AUR_HOME:?}/$package"
      fi
    )
  done

  _print_final_effect_msg "Installed" "${aurPackages[@]}"
}

# Remove one or more installed AUR packages.
aur-remove() {
  # Get args from stdin if none are passed as args
  set -- ${*:-$(</dev/stdin)}

  local -a aurPackages=("$@")
  local -a removedPackages=()
  local package

  _check_aur_provided "${aurPackages[@]}" || return 1

  for package in "${aurPackages[@]}"; do
    if ! _check_aur_installed "$package"; then
      continue
    fi

    log_info "Removing AUR: $package"
    sudo pacman -R "$package"
    log_info "Removing package directory"
    rm -rf "${AUR_HOME:?}/${package}"
    removedPackages+=("$package")
  done

  _print_final_effect_msg "Removed" "${removedPackages[@]}"
}

aur-update-packages() {
  # Get args from stdin if none are passed as args
  set -- ${*:-$(</dev/stdin)}

  local -a aurPackages=("$@")
  # Array of packages successfully updated at the end.
  local -a updatedPackages=()
  local package

  _check_aur_provided "${aurPackages[@]}" || return 1

  [ -z "$AUR_HOME" ] && AUR_HOME="$HOME/.aur"

  for package in "${aurPackages[@]}"; do
    [ "$(_check_aur_installed "$package")" == true ] || continue

    log_info "Updating AUR: $package"
    (
      cd "${AUR_HOME}/${package}"
      _prepare_and_build_package && updatedPackages+=("$package")
    )
  done

  _print_final_effect_msg "Updated" "${updatedPackages[@]}"
}

aur-update-all() {

  log_info "Updating all installed AUR packages"
  aur-update-packages $(find "$AUR_HOME" -maxdepth 1 -mindepth 1 -type d |
  sed 's|.*/||' | tr '\n' ' ')
}

aur-update() {
  local packageName="$*"

  if [ "$(echo "$packageName" | tr '[:upper:]' '[:lower:]')" == "all" ]; then
    aur-update-all
  else
    aur-update-packages "$packageName"
  fi
}

# Install an AUR package manager for the Arch Linux User Repository.
install_aur_helper() {
  if [ ! -x "$(command -v "${AUR_HELPER}")" ]; then
    aur install "${AUR_HELPER}"
  else
    warn "AUR helper ${BLUE}${AUR_HELPER}${NC} already installed."
  fi
}

# Install all AUR packages
install_aur_packages() {
  if [ ! -x "$(command -v "${AUR_HELPER}")" ]; then
    install_aur_helper
  fi
  aurhinc $(grep -E -v "^\s*#" "${AUR_PACKAGES_FILE}" | tr '\n' ' ')
}

get_aur_packages() {
  aur get <"$DOTFILES_HOME/init/package_files/aur_packages.txt"
}

_prepare_aur_environment() {
  if [ -z "$AUR_HOME" ]; then
    export AUR_HOME="$HOME/.aur"
  fi
}

_check_aur_provided() {
  if [ "$#" -lt 1 ]; then
    err "No AUR package(s) provided."
    return 1
  fi
}

_check_aur_installed() {
  local package="$1"

  if [ ! -d "${AUR_HOME}/${package}" ]; then
    warn "Package ${package} not found; skipping"
    return 1
  fi
}

# Clean package directory and run the makepkg command.
_prepare_and_build_package() {
  local hasStashedChanges=false

  git clean -f &>/dev/null
  git reset --hard HEAD . &>/dev/null
  if ! git diff-index --quiet HEAD --; then
    git stash save &>/dev/null
    hasStashedChanges=true
  fi
  git pull &>/dev/null

  {
    makepkg -sic --noconfirm &&
    [ $hasStashedChanges ] &&
    git stash pop || :
    } || {
    err "Problem building package $(basename "$(pwd)")"
    return 1
  }
}

_print_final_effect_msg() {
  local effect="$1"
  shift
  local -a packages=("$@")

  if [ "${#packages[@]}" -gt 0 ]; then
    echoe ""
    succ "${effect}: ${packages[*]}"
  fi
}

# Filters a list of requested packages based on whether or not they are already
# present in the AUR_HOME directory.
_get_list_of_new_packages() {
  local -a aurPackages=("$@")
  local -a newPackages=()
  local package

  for package in "${aurPackages[@]}"; do
    if [ ! -d "${AUR_HOME}/${package}" ]; then
      newPackages+=("$package")
    else
      log_info "Package $package already present in ${AUR_HOME}; skipping"
    fi
  done

  echo "${newPackages[@]}"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
