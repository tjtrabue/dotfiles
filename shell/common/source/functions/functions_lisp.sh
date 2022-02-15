#!/bin/sh

# Install lisp-format, a similar program to clang-format for Lisp code.
install_lisp_format() {
  local downloadUrl="https://raw.githubusercontent.com/eschulte/lisp-format/master/lisp-format"
  local installPrefix="${HOME}"
  local installDir="${installPrefix}/bin"

  log_info "Installing lisp-format script to: ${BLUE}${installDir}${NC}"
  curl -fsSLO --create-dirs --output-dir "${installDir}" "${downloadUrl}" &&
    chmod 755 "${installDir}/$(basename "${downloadUrl}")" ||
    {
      err "Could not download lisp-format from location:" \
        "${GREEN}${downloadUrl}${NC}"
      return 1
    }
}

# SBCL does not ship with readline support, so we'll need to wrap the REPL
# command with the `rlwrap` command to inject readline support.
sbcl() {
  if [ -z "$(command -v rlwrap)" ]; then
    err "rlwrap not installed"
    return 1
  fi

  if [ -n "$(command -v ros)" ]; then
    # Use Roswell if available.
    rlwrap ros run
  else
    # Fall back on a standard SBCL installation.
    rlwrap sbcl
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
