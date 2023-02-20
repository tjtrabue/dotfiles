#!/bin/sh

# Install the new-fangled digestif LSP server for LaTeX. It's quite cool!
#
# Note for Emacs users:
# If you use `lsp-mode' as your LSP client, you may need to restart Emacs a few
# times before lsp-mode picks up on the fact that digestif exists on your path.
install_digestif_lsp() {
  local digestifWrapperUrl="https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif"
  local prefix="${HOME}/.local"
  local destDir="${prefix}/bin"
  local digestifPath="${destDir}/digestif"

  if [ -x "${digestifPath}" ]; then
    warn "${CYAN}digestif${NC} wrapper already exists in expected location." \
      "Exiting."
    return 1
  fi

  log_info "Installing digestif wrapper script to: ${BLUE}${digestifPath}${NC}"
  mkdir -p "${destDir}"
  curl -sL -o "${digestifPath}" -- "${digestifWrapperUrl}"
  chmod +x "${digestifPath}"

  # Run the downloaded wrapper script to finish the installation.
  # You'll have to quit out of this interactive process manually with C-c.
  log_info "Finishing digestif installation"
  digestif
}

# Intall the latest TeX Live distribution on a UNIX OS. This is the preferred
# approach to installing TeX Live, since system package repositories usually
# contain out-of-date versions that are incompatible with the latest TeX Live
# package repositories, meaning you cannot automatically install package using
# `tlmgr install <package>'.
#
# To uninstall this distribution, the cleanest solution is to use
# `sudo tlmgr remove --all --force'. If that does not work, the directories you
# must remove will be named with the scheme:
#   /usr/local/texlive/YYYY
#   ~/.texliveYYYY
install_latest_texlive_distribution() {
  local tempDir="/tmp"
  local texliveDownloadUrl="https://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz"
  local tarFileName="install-tl-unx.tar.gz"

  log_info "Installing latest TeX Live distribution"
  (
    cd "${tempDir}" && \
    wget "${texliveDownloadUrl}" && \
    zcat "${tarFileName}" | tar xf - && \
    (
      cd "$(find "${tempDir}" -maxdepth 1 -mindepth 1 \
        -type d -name 'install-tl-*')" && \
      sudo perl ./install-tl --no-interaction
    )
  ) && \
  rm -rf "${tempDir:?}"/install-tl-* && \
  cat <<EOF
Done! Do not forget to prepend this to your PATH:
  /usr/local/texlive/YYYY/bin/PLATFORM,
You will also want to add these to your mandb config file (/etc/manpath.config):
  /usr/local/texlive/YYYY/texmf-dist/doc/man
and your INFOPATH file (~/.info_path):
  /usr/local/texlive/YYYY/texmf-dist/doc/info
EOF
}

# vim:foldenable:foldmethod=indent:foldnestmax=1