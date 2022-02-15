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

# Launch default Lisp REPL.
lisp() {
  if [ -n "$(command -v rlwrap)" ]; then
    # Most Lisp REPLs do not support readline. We want to wrap the REPL command
    # with the `rlwrap` utility that provides readline for us.
    eval "rlwrap $(__get_lisp_run_command)"
  else
    # Default to simply using SBCL without rlwrap if we do not have rlwrap
    # installed.
    eval "$(__get_lisp_run_command)"
  fi
}

# Figure out which command line to execute in order to summon up a Lisp REPL.
__get_lisp_run_command() {
  local defaultLispRunCommand="$(__get_default_lisp_run_cmd)"

  if [ -n "$(command -v ros)" ]; then
    # Use Roswell if available.
    printf "ros run"
  else
    printf "${defaultLispRunCommand}"
  fi
}

# Return the default Common Lisp implementation to use.
__get_default_lisp_run_cmd() {
  printf "sbcl"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
