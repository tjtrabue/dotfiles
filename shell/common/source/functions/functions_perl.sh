#!/bin/sh

# Install cpanm (CpanMinus) for the current user.
bootstrap_cpanm() {
  local cpanMinusUrl="https://cpanmin.us"

  log_info "Bootstrapping cpanm for user: ${GREEN}${USER}${NC}"
  curl -sL "${cpanMinusUrl}" | perl - "App::cpanminus"
}

# Install plsense intellisense engine for Perl.
# As of now it is not on CPAN.
install_plsense() {
  local version="${1:-0.3.4}"
  # URL to the release page for plsense.
  local base_url="https://github.com/aki2o/plsense/releases/download"
  # Full download URL.
  local full_url="${base_url}/v${version}/PlSense-${version}.tar.gz"

  if cpanm -i --force "$full_url"; then
    # Add plsense configuration after installing.
    plsense config
  fi
}

# Install all global Perl packages.
install_perl_packages() {
  local package

  if [ ! -f "${PERL_PACKAGES_FILE}" ]; then
    err "No Perl packages file found."
    return 1
  fi

  # Make sure cpanm is installed
  if [ ! -x "$(command -v cpanm)" ]; then
    bootstrap_cpanm
  fi

  log_info "Installing Perl global packages"
  while IFS="" read -r package || [ -n "${package}" ]; do
    cpanm -i --force "${package}"
  done <"${PERL_PACKAGES_FILE}"

  install_plsense
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
