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

# Install all global Perl packages from CPAN.
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

  log_info "Installing Perl global packages from CPAN"
  install_packages_from_file_with_tool "cpanm -i --force" \
    "${PERL_PACKAGES_FILE}"

  install_plsense
}

# Update all Perl packages installed from CPAN.
# Requires the `cpan-outdated` utility.
update_perl_packages() {
  if [ -z "$(command -v cpanm)" ]; then
    err "No cpanm (CPANMinus) executable found on PATH"
    return 1
  fi
  if [ -z "$(command -v cpan-outdated)" ]; then
    err "Must install cpan-outdated in order to update Perl packages"
    return 2
  fi
  log_info "Updating Perl global packages from CPAN"
  cpan-outdated -p | cpanm --force
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
