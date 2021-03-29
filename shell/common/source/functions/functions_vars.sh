#!/bin/sh

# Re-copy the .vars file from the dotfiles repository to the user's home
# directory.
sync_vars() {
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local dotVars="${dotCopy}/.vars"
  local userVars="${HOME}/.vars"

  if [ ! -f "${dotVars}" ]; then
    err "No .vars file found in: ${dotCopy}"
    return 1
  fi

  log_info "Copying dotfiles vars file ${dotVars} to ${userVars}"
  cp -f "${dotVars}" "${userVars}"
  __add_extra_os_vars
}

# Inject extra environment variables into the ~/.vars file, if prudent.
__add_extra_os_vars() {
  local os="$(getosinfo | head -1 | sed 's/Distribution:\s*//')"
  local dotCopy="${DOTFILES_COPY:-${DOTFILES_HOME}/copy}"
  local extraVarsLinuxDir="${dotCopy}/var_files/linux"
  local markerString="#<additional-vars-insert>"
  local extraVarsFile="NONE"

  log_info "Injecting additional OS variables into ${TARGET_HOME}/.vars"

  case "${os}" in
    "Arch Linux")
      extraVarsFile="${extraVarsLinuxDir}/arch_vars.bash"
      ;;

    *)
      log_info "No extra vars to add for OS: ${os}"
      ;;
  esac

  if [ -f "${extraVarsFile}" ]; then
    sed -i -e "/${markerString}/r ${extraVarsFile}" "${TARGET_HOME}/.vars"
  fi

  # Get rid of marker string in ~/.vars
  sed -i "/${markerString}/d" "${TARGET_HOME}/.vars"
  log_info "Done injecting additional variables"
}
