#!/bin/sh

# Prepare Python environment for the current shell.
src_python_for_profile() {
  local pyenvHome="${PYENV_ROOT:-${HOME}/.pyenv}"

  if ! tool_installed "pyenv" "${pyenvHome}"; then
    install_or_update_pyenv
    install_latest_python
  fi

  if [ "$(command -v pyenv)" != "" ]; then
    # Add shims to PATH
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
  fi
}

# Installs all packages listed in the pyhon2 and python3 package lists
install_python_packages() {
  # NOTE: Python 2.x has reached its end-of-life, and is no longer supported.
  if __python2_pip_installed; then
    __install_python2_packages
  fi
  if __python3_pip_installed; then
    __install_python3_packages
  fi
}

# Update all installed Python packages.
# Handles Python 2.X and Python 3.Y.
update_python_packages() {
  # NOTE: Python 2.x has reached its end-of-life, and is no longer supported.
  if __python2_pip_installed; then
    __update_python2_packages
  fi
  if __python3_pip_installed; then
    __update_python3_packages
  fi
}

# Install or update pyenv for managing python versions.
install_or_update_pyenv() {
  local pyenvDir="${PYENV_DIR:-${HOME}/.pyenv}"
  if [ -d "${pyenvDir}" ]; then
    __update_pyenv
  else
    __install_pyenv
  fi
}

# Install the latest version of Python with pyenv.
install_latest_python() {
  local latestPythonVersion="$(pyenv install --list |
    sed 's/^\s*//' | grep '^3' | tail -1)"

  log_info "Installing ${YELLOW}Python ${latestPythonVersion}${NC} with pyenv"
  pyenv install -s "${latestPythonVersion}" &&
    pyenv global "${latestPythonVersion}"
}

get_python2_version() {
  python2 --version 2>&1 | awk '{print $2}'
}

get_python3_version() {
  python3 --version 2>&1 | awk '{print $2}'
}

# Install pyenv for managing python environments.
__install_pyenv() {
  local pyenvHome="${PYENV_DIR:-${HOME}/.pyenv}"

  install_tool_from_url_and_script "pyenv" "${pyenvHome}" \
    "https://pyenv.run"
}

__update_pyenv() {
  log_info "Updating pyenv to latest version"
  pyenv update
}

__install_python2_packages() {
  log_info "Installing Python 2.x packages"
  if [ -f "${PYTHON2_PACKAGES_FILE}" ]; then
    python2 -m pip install --user --upgrade -r "${PYTHON2_PACKAGES_FILE}"
  else
    err "Could not find Python 2 package file."
    return 1
  fi
}

__install_python3_packages() {
  log_info "Installing Python 3.y packages"
  if [ -f "${PYTHON3_PACKAGES_FILE}" ]; then
    python3 -m pip install --user --upgrade -r "${PYTHON3_PACKAGES_FILE}"
  else
    err "Could not find Python 3 package file."
    return 1
  fi
}

__update_python2_packages() {
  log_info "Updating Python 2.x packages"
  pip2 freeze --local | grep -v '^\-e' |
    cut -d = -f 1 | xargs -n1 pip2 install --user --upgrade
}

__update_python3_packages() {
  log_info "Updating Python 3 packages"
  python3 -m pip list --outdated --format=freeze | grep -v '^\-e' |
    cut -d = -f 1 | xargs -n1 python3 -m pip install --user --upgrade
}

# Return 0 if PIP installed for Python 2.X
__python2_pip_installed() {
  python2 -m pip --version >>/dev/null 2>&1
}

# Return 0 if PIP installed for Python 3.Y
__python3_pip_installed() {
  python3 -m pip --version >>/dev/null 2>&1
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
