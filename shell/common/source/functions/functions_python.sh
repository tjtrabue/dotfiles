#!/bin/sh

# Installs all packages listed in the pyhon2 and python3 package lists
install_python_packages() {
  # NOTE: Python 2.x has reached its end-of-life, and is no longer supported.
  # __install_python2_packages
  __install_python3_packages
}

__install_python2_packages() {
  log_info "Installing Python 2.x packages"
  if [ -f "${PYTHON2_PACKAGES_FILE}" ]; then
    python2 -m pip install --user --upgrade -r "${PYTHON2_PACKAGES_FILE}"
  else
    err "Could not file Python 2 package file"
    return 1
  fi
}

__install_python3_packages() {
  log_info "Installing Python 3.x packages"
  if [ -f "${PYTHON3_PACKAGES_FILE}" ]; then
    python3 -m pip install --user --upgrade -r "${PYTHON3_PACKAGES_FILE}"
  else
    err "Could not file Python 3 package file"
    return 1
  fi
}

update_python_packages() {
  log_info "Updating Python 2 packages"
  pip2 freeze --local | grep -v '^\-e' |
  cut -d = -f 1 | xargs -n1 pip2 install --user --upgrade

  log_info "Updating Python 3 packages"
  python3 -m pip list --outdated --format=freeze | grep -v '^\-e' |
  cut -d = -f 1 | xargs -n1 python3 -m pip install --user --upgrade
}

# Install pyenv for managing python environments.
install_pyenv() {
  local pyenvHome="${PYENV_DIR:-${HOME}/.pyenv}"

  __install_tool_from_url_and_script "pyenv" "${pyenvHome}" \
    "https://pyenv.run"
}

# Prepare Python environment for the current shell.
src_python_for_profile() {
  local pyenvHome="${PYENV_ROOT:-${HOME}/.pyenv}"

  if ! __tool_installed "pyenv" "${pyenvHome}"; then
    install_pyenv
  fi

  if [ "$(command -v pyenv)" != "" ]; then
    # Add shims to PATH
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
  fi
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
