#!/bin/sh

# Installs a python PIP package for both Python v2 and v3
smartpip() {
  local writeToPackageFiles=false;
  local getopt_results
  local packages;
  local package;

  getopt_results=$(getopt -o w --long write-to-file -- "$@")
  eval set -- "$getopt_results"

  while true; do
    case "$1" in
      "-w" | "--write-to-file")
        writeToPackageFiles=true;
        shift;
        ;;

      --)
        shift;
        break;
        ;;

      *)
        err "Unknown argument $1 to ${FUNCNAME[0]}";
        return 1;
        ;;
    esac
  done

  packages=("$@");
  if [[ "${#packages[@]}" -lt 1 ]]; then
    err "No package(s) provided to smartpip";
    return 2;
  fi

  if $writeToPackageFiles; then
    # Install all packages and write their names as dependencies in the Python user package files
    for package in "${packages[@]}"; do
      # Make sure each package install correctly before writing it to the package file
      pip2 install --user --upgrade "$package" && \
        echo "$package" >> "$PYTHON2_PACKAGES_FILE"
      pip3 install --user --upgrade "$package" && \
        echo "$package" >> "$PYTHON3_PACKAGES_FILE";
    done

    # Re-sort the packages files after adding the new entries
    sort -u -o "$PYTHON2_PACKAGES_FILE" "$PYTHON2_PACKAGES_FILE";
    sort -u -o "$PYTHON3_PACKAGES_FILE" "$PYTHON3_PACKAGES_FILE";
  else
    # Just install the packages if the user did not supply the "--write-to-file" flag
    pip2 install --user --upgrade "${packages[@]}"
    pip3 install --user --upgrade "${packages[@]}";
  fi
}

# Installs all packages listed in the pyhon2 and python3 package lists
install_python_packages() {
  if [ -f "${PYTHON2_PACKAGES_FILE}" ]; then
    python2 -m pip install --user --upgrade -r "${PYTHON2_PACKAGES_FILE}";
  else
    err "Could not file Python 2 package file";
    return 1;
  fi

  if [ -f "${PYTHON3_PACKAGES_FILE}" ]; then
    python3 -m pip install --user --upgrade -r "${PYTHON3_PACKAGES_FILE}";
  else
    err "Could not file Python 3 package file";
    return 2;
  fi
}

update_python_packages() {
  log_info "Updating Python 2 packages"
  pip2 freeze --local | grep -v '^\-e' \
    | cut -d = -f 1  | xargs -n1 pip2 install --user --upgrade

  log_info "Updating Python 3 packages"
  python3 -m pip list --outdated --format=freeze | grep -v '^\-e' \
    | cut -d = -f 1  | xargs -n1 python3 -m pip install --user --upgrade
}

# Install pyenv for managing python environments.
install_pyenv() {
  local pyenvHome="${PYENV_DIR:-${HOME}/.pyenv}"

  __install_tool_from_url_and_script "pyenv" "${pyenvHome}" \
    "https://pyenv.run"
}

# Prepare Python environment for the current shell.
src_python_for_profile() {
  local pyenvHome="${PYENV_DIR:-${HOME}/.pyenv}"

  if ! __tool_installed "pyenv" "${pyenvHome}"; then
    install_pyenv
  fi

  if [ "$(command -v pyenv)" != "" ]; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
