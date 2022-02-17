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

# Launch default Common Lisp REPL.
lisp() {
  local lispRunCommand="$(__get_lisp_run_command)"

  if [ -z "${lispRunCommand}" ]; then
    err "Could not determine Lisp run command"
    return 1
  fi

  if [ -n "$(command -v rlwrap)" ]; then
    # Most Lisp REPLs do not support readline. We want to wrap the REPL command
    # with the `rlwrap` utility that provides readline capabilities.
    lispRunCommand="rlwrap ${lispRunCommand}"
  fi

  eval "${lispRunCommand}"
}

# Figure out which command line to execute in order to summon up a Lisp REPL.
__get_lisp_run_command() {
  if [ -n "$(command -v ros)" ]; then
    # Use Roswell if available.
    printf "ros run"
  else
    __get_default_common_lisp_program
  fi
}

# Return the default Common Lisp implementation to use.
__get_default_common_lisp_program() {
  if [ -x "$(command -v sbcl)" ]; then
    printf "sbcl"
  else
    err 'No Common Lisp installation found on $PATH'
    return 1
  fi
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
